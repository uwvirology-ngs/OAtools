"""
This python script houses the internal logic computing the parameters and features of 5-parameter logistic and linear
regression models fit to two-dimensional multicomponent fluorescence vs. cycle number data. The fit_curve() function 
found here powers the run_fit_curve() R function of the package. 
"""

# --------------------------------------------------------- Imports --------------------------------------------------------

import numpy as np
import pandas as pd
from scipy.optimize import curve_fit
from typing import Union, Any

# ---------------------------------------------------- Regression Helpers --------------------------------------------------

# linear regression equation
def linear_regression(x: Union[np.ndarray, float], M: float, C: float) -> float:
    return M * x + C

# 5-parameter logistic regression equation
def logistic_regression(x: Union[np.ndarray, float], A: float, D: float, C: float, B: float, S: float) -> float:
    return A + (D - A) / ((1 + np.exp(B * (C - x))) ** S)

# computes and returns the cycle point at which fluorescence crosses the midpoint between its overall minima and maxima
def midpoint_5pl(C: float, B: float, S: float) -> float:
    return C - (np.log(2**(1 / S) - 1)) / B

# computes and returns the derivative of the 5pl regression equation with respect to x, the cycle number
def derivative_5pl(x: float, A: float, D: float, C: float, B: float, S: float) -> float:
    z: float = 1 + np.exp(B * (C - x))                                                    # inner term
    derivative: float = (D - A) * S * z**(-S - 1) * B * np.exp(B * (C - x))               # overall derivative
    return derivative

# computes and returns the r_squared value between predicted and observed fluorescence values
def compute_r_squared(y_data: np.ndarray, y_pred: np.ndarray) -> float:
   residuals: np.ndarray = y_data - y_pred
   ss_res = np.sum(residuals**2)
   ss_tot = np.sum((y_data - np.mean(y_data))**2)

   if ss_tot == 0:
      return 0.0

   r_squared = 1 - (ss_res / ss_tot)
   return(float(r_squared))

# -------------------------------------------------------- Curve Fitting ---------------------------------------------------


def fit_curve(data: pd.DataFrame, linear_threshold: float) -> dict[str, Any]:
    """
    Optimizes the fit between two-dimensional fluorescence vs. cycle number data and a 5-parameter logistic regression. Defaults 
    to a linear regression instead in the case the overall change in fluorescence is below the given threshold value, or the
    optimization to a 5-parameter logistic curve fails. 

    Args:
        data (pd.DataFrame): data for a single reaction, expected colnames are 'fluo' and 'cycle'
        linear_threshold (float): minimum overall change in fluorescence to attempt fitting a 5pl regression

    Returns: 
        dict[Any]: dictionary summarizing curve-fitting results and metadata, passed to R via reticulate
    """
    
    # precondition checks
    if not {'cycle', 'fluo'}.issubset(data.columns):
      raise ValueError("missing required columns, ensure data frame passed to fit_curve contains 'cycle' and 'fluo' columns.")
    
    if data['cycle'].duplicated().any():
      raise ValueError("duplicate values present in cycle column. likely cause is several runs of data passed to fit_curve function.")
    
    # convert run data to numpy array
    x_data: np.ndarray = data['cycle'].to_numpy()                   # load fluo/cycle data as numpy arrays
    y_data: np.ndarray = data['fluo'].to_numpy()
    delta: float = np.mean(y_data[-3:]) - np.mean(y_data[:3])       # compute overall change in fluorescence as difference from final three to first three cycles
    
    # engine for fitting 5pl equation
    def fit_5pl() -> tuple[np.ndarray, np.ndarray, np.ndarray]:
        initial_guess: list = [7000, 14000, 20, 0.1, 1.0]           # initial guess for parameters to speed along optimizer
        bounds: tuple[list[float], list[float]] = (                 # bounds for model parameters to guard against overflow
          [0, 0, 1, -10, 0.1],
          [20000, 20000, 40, 10, 10]
        )
        params, cov = curve_fit(logistic_regression, x_data, y_data, p0 = initial_guess, bounds = bounds)
        y_pred: np.ndarray = logistic_regression(x_data, *params)
        return params, cov, y_pred 

    # engine for fitting linear equation
    def fit_linear() -> tuple[np.ndarray, np.ndarray, np.ndarray]: 
        params, cov = curve_fit(linear_regression, x_data, y_data)
        y_pred: np.ndarray = linear_regression(x_data, *params)
        return params, cov, y_pred
    
    # select model for optimization
    if delta < linear_threshold:                                  # if overall change in fluorescence is lesser than the threshold, default to a linear model
        params, cov, y_pred = fit_linear()
        regression_type: str = "lin"
    else:                                                           # otherwise, attempt to fit a logistic model
        try:
          params, cov, y_pred = fit_5pl()
          regression_type: str = "5pl"
        except RuntimeError:                                        # if the optimizer fails, fall back to a linear model
          params, cov, y_pred = fit_linear()
          regression_type: str = "lin"
    
    # save model features
    if regression_type == "5pl":  
        A, D, C, B, S = params
        coefficients: dict[str, float] = {"A_fit": A, "D_fit": D, "B_fit": B, "C_fit": C, "S_fit": S}
        x_mid: float = midpoint_5pl(C, B, S)
        y_mid: float = logistic_regression(x_mid, A, D, C, B, S)
        slope: float = derivative_5pl(x_mid, A, D, C, B, S)
    else:      
        M, C_ = params
        coefficients: dict[str, float] = {"M_fit": M, "C_fit": C_}
        x_mid: float = x_data.max() / 2
        y_mid: float = linear_regression(x_mid, M, C_)
        slope: float = M
    
    # generate and return model features and metadata
    model: dict[str, Any] = {
        "regression": regression_type,
        "r_squared": compute_r_squared(y_data, y_pred),
        "delta": delta,
        "x_data": x_data,
        "y_data": y_data,
        "y_pred": y_pred,
        "coefficients": coefficients,
        "x_mid": x_mid,
        "y_mid": y_mid,
        "slope": slope
    }
    
    return model
