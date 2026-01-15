"""
Computing predicted fluorescence values from cycle numbers and logistic regression equations.  
Provides helper functions for computing features such as slope and reaction midpoint. 
"""

import numpy as np
from typing import Union, overload

@overload
def five_param_logistic_regression(x: float, A: float, D: float, B: float, C: float, S: float) -> float: ...

@overload
def five_param_logistic_regression(x: np.ndarray, A: float, D: float, B: float, C: float, S: float) -> np.ndarray: ...

def five_param_logistic_regression(x: Union[np.ndarray, float], A: float, D: float, B: float, C: float, S: float) -> Union[np.ndarray, float]:
    """
    Compute logistic regression: y = A + (D - A) / ((1 + e^(B * (C - x)))^S)
    
    Args:
        x: cycle number
        A: horizontal asymptote as x approaches negative infinity
        D: horizontal asymptote as x approaches positive infinity
        B: parameter for steepness of the curve
        C: parameter for location, midpoint between asymptotes when S = 1
        S: parameter for asymmetry of the curve
    
    Returns:
        predicted fluorescence value(s)
    """
    return A + (D - A) / ((1 + np.exp(B * (C - x))) ** S)

def midpoint_5pl(B: float, C: float, S: float) -> float:
    """
    Compute cycle at which fluorescence crosses between horizontal asymptotes

    Args:
        B: parameter for steepness of the curve
        C: parameter for location, midpoint between asymptotes when S = 1
        S: parameter for asymmetry of the curve
    
    Returns:
        cycle number
    """
    return C - (np.log(2**(1 / S) - 1)) / B

# computes and returns the derivative of the 5pl regression equation with respect to x, the cycle number
def derivative_5pl(x: float, A: float, D: float, B: float, C: float, S: float) -> float:
    """
    Compute value of the derivative of the 5pl regression with respect to cycle number
    
    Args: 
        x: cycle number
        A: horizontal asymptote as x approaches negative infinity
        D: horizontal asymptote as x approaches positive infinity
        B: parameter for steepness of the curve
        C: parameter for location, midpoint between asymptotes when S = 1
        S: parameter for asymmetry of the curve
    
    Returns:
        value of the derivative at the given cycle number
    """
    z: float = 1 + np.exp(B * (C - x))
    derivative: float = (D - A) * S * z**(-S - 1) * B * np.exp(B * (C - x))
    return derivative