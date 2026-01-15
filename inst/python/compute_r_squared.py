"""
Computing r_squared values from predicted and observed fluorescence curves.
"""

import numpy as np

def compute_r_squared(y_data: np.ndarray, y_pred: np.ndarray) -> float:
    """
    Compute r_squared value

    Args:
        y_data: observed fluorescence values
        y_pred: predicted fluorescence values
    
    Returns: 
        r_squared value
    """
    residuals = y_data - y_pred
    
    ss_res = np.sum(residuals**2)                   # residual sum of squares
    ss_tot = np.sum((y_data - np.mean(y_data))**2)  # total sum of squares

    if ss_tot == 0:
        return 0.0

    return(1 - (ss_res / ss_tot))