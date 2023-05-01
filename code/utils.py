import pandas as pd

def filter_and_remove(df, name):
    # filter the dataframe based on the "name" column matching the specified value
    filtered_df = df[df['name'] == name].copy()

    # remove the "name" column from the filtered dataframe
    filtered_df = filtered_df.drop('name', axis=1)

    return filtered_df

def ends_with_number(column_name, numbers):
    for number in numbers:
        if column_name.endswith(str(number)):
            return True
    return False

def change_id(df):
    # create a new dataframe with the same data as the input dataframe
    new_df = df.copy()

    # create a dictionary to map periodIDs to integer values
    period_dict = {period: i for i, period in enumerate(sorted(new_df['periodID'].unique()))}

    # map the periodIDs to integer values and assign them to the "periodID" column
    new_df['periodID'] = new_df['periodID'].map(period_dict) + 1

    return new_df