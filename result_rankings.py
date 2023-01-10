import pandas as pd
import numpy as np

df = pd.read_excel(r"C:\Users\sara.brumfield2\Downloads\ResultTeams_Data_8184660_1673380599.xlsx")

numerics = ['int16', 'int32', 'int64', 'float16', 'float32', 'float64']
cols = df.select_dtypes(include=numerics).columns.to_list()
cols.remove('Unique ID')
cols.append('Select the service of the proposal')
cols.append('Focus area of the proposal')

data = df[df.columns.intersection(cols)]

data = data.rename(columns = {"Focus area of the proposal":"Focus", "Select the service of the proposal": "Service", "The proposal has a realistic and accurate budget for the proposed level of service.": "Stewardship", "The proposal has a strong and clear plan for addressing disparities in the communities they serve.":"Equity", "The proposal has a feasible and responsible plan to adapt to changing community conditions.":"Resiliency", "The proposal demonstrates the service's ability to maintain or improve service levels at the proposed funding level.":"Impact"})

data["Action Plan"] = data.loc[:, ["The proposal demonstrates a strong and clear link to both a goal and action in the Mayor's Action Plan.", "The proposal has a realistic plan for achieving progress on the goal and action."]].mean(axis = 1, skipna = True)
data["Workload Delivery"] = data.loc[:, ["The proposal clearly explains the problem and proposes a responsible solution.", "The proposal outlines the specific improvements or efficiencies in the proposed solution."]].mean(axis = 1, skipna = True)
data = data.drop(columns = ["The proposal demonstrates a strong and clear link to both a goal and action in the Mayor's Action Plan.", "The proposal has a realistic plan for achieving progress on the goal and action.", "The proposal clearly explains the problem and proposes a responsible solution.", "The proposal outlines the specific improvements or efficiencies in the proposed solution."], axis = 1)

numerics = ['int16', 'int32', 'int64', 'float16', 'float32', 'float64']
num_cols = data.select_dtypes(include=numerics).columns.to_list()

data["Avg"] = data.loc[:, num_cols].mean(axis = 1, skipna = True)
data["Sum"] = data.loc[:, num_cols].sum(axis = 1, skipna = True)
data["%"] = round((data["Sum"] / 25), 3) * 100

new_col_ord = ['Service',  'Focus', 'Stewardship', 'Equity', 'Resiliency', 'Impact', 'Action Plan', 'Workload Delivery', 'Avg', 'Sum', '%']

data = data[new_col_ord]

rank = data.sort_values(by = "%", ascending = False)

##aggregate

focus_area = data.groupby(["Focus"], as_index = False)['Stewardship', 'Equity', 'Resiliency', 'Impact', 'Action Plan', 'Workload Delivery'].mean()

services = data["Service"].unique()
len(services)

data.groupby("Focus", as_index = False)["Service"].count()
