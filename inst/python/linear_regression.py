"""
Computing predicted fluorescence values from cycle numbers and linear regression equations.  
"""

import numpy as np
from typing import Union, overload

@overload
def linear_regression(x: float, M: float, C: float) -> float: ...

@overload
def linear_regression(x: np.ndarray, M: float, C: float) -> np.ndarray: ...

def linear_regression(x: Union[float, np.ndarray], M: float, C: float) -> Union[float, np.ndarray]:
    """
    Compute linear regression: y = Mx + C
    
    Args:
        x: cycle number(s)
        M: slope
        C: y-intercept
    
    Returns:
        predicted fluorescence value(s)
    """
    return M * x + C