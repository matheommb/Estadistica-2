import pandas as pd
import numpy as np
from itertools import combinations
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error


def myAllRegTable(df, target_col):
    cols = list(df.columns)
    cols.remove(target_col)
    all_combinations = [c for i in range(
        len(cols)+1) for c in combinations(cols, i)]
    all_combinations = [c for c in all_combinations if len(c) > 0]

    n = len(df)
    X_full = df[cols]
    y_full = df[target_col]
    lm_full = LinearRegression()
    lm_full.fit(X_full, y_full)
    y_pred_full = lm_full.predict(X_full)
    mse_full = mean_squared_error(y_full, y_pred_full)
    # sse_full = np.sum((y_full - y_pred_full)**2)

    reg_results = []
    for comb in all_combinations:
        X = df[list(comb)]
        y = df[target_col]
        lm = LinearRegression()
        lm.fit(X, y)
        y_pred = lm.predict(X)
        r_sq = lm.score(X, y)
        adj_r_sq = 1 - (1-r_sq)*(len(y)-1)/(len(y)-X.shape[1]-1)
        mse = mean_squared_error(y, y_pred)
        sse_p = np.sum((y - y_pred)**2)
        cp = (sse_p / mse_full) - (n - 2*(len(comb) + 1))
        cp_p = cp - (len(comb) + 1)
        reg_results.append([len(comb), r_sq, adj_r_sq, mse, cp, cp_p, comb])

    col_names = ['k', 'R_sq', 'adj_R_sq', 'MSE',
                 'Cp', 'Cp_p', 'Variables_in_model']
    result = pd.DataFrame(reg_results, columns=col_names)
    result = result.drop_duplicates(
        subset=['Variables_in_model']).reset_index(drop=True)
    return result
