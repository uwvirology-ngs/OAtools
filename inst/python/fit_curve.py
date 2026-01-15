"""
Selecting and optimizing model equations to PCR fluorescence measurements.
"""

import numpy as np
import pandas as pd
from scipy.optimize import curve_fit
from typing import Any

from linear_regression import linear_regression
from five_param_logistic_regression import (
    five_param_logistic_regression,
    derivative_5pl,
    midpoint_5pl
)
from compute_r_squared import compute_r_squared

def fit_curve(pcr_data: pd.DataFrame, linear_threshold: float) -> dict[str, Any]:
    """
    Selects and optimizes a model equation, either logistic or linear, to
    fluorescence values measured over the course of a PCR reaction. 

    Attempts optimization to a 5-parameter logistic regression when overall
    change in fluorescence exceeds a predefined threshold. Falls back to a 
    linear regression when either the threshold is not met or in case the 
    optimization algorithm fails. 

    Args:
        pcr_data: DataFrame containing PCR data with 'fluo' and 'cycle' columns
        linear_threshold: minimum overall change in fluorescence to attempt optimization to a logistic regression

    Returns:
        dictionary with regression equation parameters, computed features, and other metadata
    """
    
    # input validation
    if not {'cycle', 'fluo'}.issubset(pcr_data.columns):
        raise ValueError("Missing required columns: data must contain 'cycle' and 'fluo' columns.")
    
    if pcr_data['cycle'].duplicated().any():
        raise ValueError("Duplicate cycle values detected: data must contain no duplicate cycles.")
    
    x_data = pcr_data['cycle'].to_numpy()                           # PCR cycles
    y_data = pcr_data['fluo'].to_numpy()                            # fluorescence measurements
    delta_fluo = float(np.mean(y_data[-3:]) - np.mean(y_data[:3]))  # total change in fluorescence
    
    # fit model equation to PCR data
    if delta_fluo >= linear_threshold:
        try:
            params, _cov = curve_fit(
                five_param_logistic_regression, x_data, y_data,
                p0 = [7000, 14000, 0.1, 20, 1.0],
                bounds = ([0, 0, -10, 1, 0.1], [20000, 20000, 10, 40, 10])
            )
            y_pred = five_param_logistic_regression(x_data, *params)
            regression_type = "5pl"
        except RuntimeError:
            params, _cov = curve_fit(linear_regression, x_data, y_data)
            y_pred = linear_regression(x_data, *params)
            regression_type = "lin"
    else:
        params, _cov = curve_fit(linear_regression, x_data, y_data)
        y_pred = linear_regression(x_data, *params)
        regression_type = "lin"
    
    # save model features
    if regression_type == "5pl":
        A, D, B, C, S = params
        coefficients = {"A": A, "D": D, "B": B, "C": C, "S": S}
        x_mid = midpoint_5pl(B, C, S)
        y_mid = five_param_logistic_regression(x_mid, A, D, B, C, S)
        slope = derivative_5pl(x_mid, A, D, B, C, S)
    else:      
        M, C_ = params
        coefficients = {"M": M, "C": C_}
        x_mid = float(x_data.max() / 2)
        y_mid = linear_regression(x_mid, M, C_)
        slope = M
    
    # return model equation, computed features, and metadata
    return {
        "regression_type": regression_type,
        "parameters": coefficients,
        "r_squared": compute_r_squared(y_data, y_pred),
        "x_obs": x_data,
        "y_obs": y_data,
        "y_pred": y_pred,
        "delta_y": delta_fluo,
        "x_midpoint": x_mid,
        "y_midpoint": y_mid,
        "slope_midpoint": slope
    }