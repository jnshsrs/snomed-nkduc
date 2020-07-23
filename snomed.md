
# Analysis of the NKDUC to SNOMED Mapping

This script contains the code and the results of our analysis for our mapping of NKDUC items to SNOMED CT.
We analysed the reliability of the mapping and the coverage rate of SNOMED CT for the NKDUC.

The information about the NKDUC and the cross map is available as csv file in the subfolder <code>./data</code>.

This script includes the following sections:
1. Load Data
2. Compute Reliability of the mapping (Fleiss Kappa)
3. Compute the Reliability of the equivalence Rating
4. Compute the Coverage Rate

## Importing and Preprocessing the Data


```python
# Mapping
import pandas as pd
from statsmodels.stats.inter_rater import fleiss_kappa
import numpy as np
res = pd.read_csv("./data/mapping-results.csv")

# res.finales_konzept = res.finales_konzept.map({1: True, 1: False}).astype('bool')
```


```python
# Recode Kapitel
chapters_de = {
    1: "Stammdaten",
    2: "Allgemeinstatus",
    3: "Wundanamnese",
    4: "Wundstatus",
    5: "Diagnostik",
    6: "Therapie",
    7: "Patient Related Outcome (PRO)",
    8: "Ernährung",
    9: "Edukation"
}

chapters_en = {
    1: "Master Patient Data",
    2: "Allgemeinstatus",
    3: "Wound Evaluation",
    4: "Wound Status",
    5: "Diagnostics",
    6: "Therapy",
    7: "Patient Related Outcome (PRO)",
    8: "Nutritional Evaluation",
    9: "Patient Education"
}


res.kapitel = res.kapitel.map(chapters_de)
res.kapitel = res.kapitel.astype('category').cat.as_ordered()
```


```python
# Compute the number of items in the chapters
n_chapter = res.loc[:, ['kapitelbezeichnung', 'id']].drop_duplicates('id').groupby('kapitelbezeichnung').count()
#n_overall = res.loc[:, ['id']].drop_duplicates('id').shape[0]
#pd.DataFrame({n_overall, index=['Overall']})
n_chapter.loc['Overall'] = n_chapter.sum(axis=0)
n_chapter.columns = ['n']
```


```python
# Equivalence Labels
equ_cat = res.equ_jens.drop_duplicates().sort_values()
iso_categories = {
    'Equivalence of meaning; lexical, as well as conceptual': 1,
    'Equivalence of meaning, but with synonymy.': 2,
    'Source concept is broader and has a less specific meaning than the target concept': 3,
    'Source concept is narrower and has a more specific meaning than the target concept': 4,
    'No map is possible': 5, 
}
```

## Reliability of the Mapping
### Proportional Agreement between Mappers


```python
mapping = res.pivot(index = "id", columns= "id_mapper", values= "snomed_code")
mapping.columns = ["map1", "map2", "map3"]

mapping['agree2'] = mapping.iloc[:, 1] == mapping.iloc[:, 2]
mapping['agree3'] = mapping.iloc[:, 0] == mapping.iloc[:, 2]
mapping['agree1'] = mapping.iloc[:, 0] == mapping.iloc[:, 1]

mapping.loc[:, 'agreement_perc'] = mapping.loc[:, ['agree1', 'agree2', 'agree3']].astype('int').sum(axis=1) / 3
mapping = res.loc[:, ['id', 'kapitelbezeichnung']].merge(right=mapping.reset_index(), on='id')

agree = mapping.loc[:, ['kapitelbezeichnung', 'agreement_perc']].groupby('kapitelbezeichnung').apply(np.mean)

agree.loc['Overall'] = mapping.agreement_perc.mean()
```

### Fleiss-Kappa


```python
def reshape_for_fleiss_kappa(data, index='id', columns='snomed_code', n_rater=3):
    """
    Restructures the dataframe to compute the Fleiss Kappa Value
    
    Parameters
    ----------
    data : pandas.DataFrame
        A dataframe with columns snomed_code, id (the item id of the nkduc items)

    Returns
    -------
    numpy.array
        A numpy array, which columns represent the snomed_ct code and rows an item of the NKDUC
        The values in the cells represent the number of raters, who chose the code for the NKDUC items
    """
    # Reshape data
    # add a column with only ones (to count the number of agreements)
    data.loc[:, 'cnt'] = np.array([1 for i in range(data.shape[0])]) 
    # To compute fleiss kappa, the data has to be reshaped into a structure 
    # in which nkduc items are in the rows and each snomed concept is in a col
    agreement = data.pivot_table(values='cnt', index=index, columns=columns, aggfunc='sum', fill_value = 0) # reshape data
    # print(np.array(agreement).sum(axis=1))
    assert all(np.array(agreement).sum(axis=1) == n_rater) # row sums should all be three (three ratings for each item)
    return agreement
```


```python
d = {}
for chapter in res.kapitelbezeichnung.drop_duplicates().values:
    df = res.loc[res.kapitelbezeichnung == chapter, :]
    fk_result = fleiss_kappa(reshape_for_fleiss_kappa(df))
    d[chapter] = fk_result
    
d['Overall'] = fleiss_kappa(reshape_for_fleiss_kappa(res))

kappa = pd.DataFrame(d, index=[0]).transpose()
kappa.columns = ['kappa']

```

    /Users/jens/anaconda3/lib/python3.7/site-packages/pandas/core/indexing.py:362: SettingWithCopyWarning: 
    A value is trying to be set on a copy of a slice from a DataFrame.
    Try using .loc[row_indexer,col_indexer] = value instead
    
    See the caveats in the documentation: http://pandas.pydata.org/pandas-docs/stable/indexing.html#indexing-view-versus-copy
      self.obj[key] = _infer_fill_value(value)
    /Users/jens/anaconda3/lib/python3.7/site-packages/pandas/core/indexing.py:543: SettingWithCopyWarning: 
    A value is trying to be set on a copy of a slice from a DataFrame.
    Try using .loc[row_indexer,col_indexer] = value instead
    
    See the caveats in the documentation: http://pandas.pydata.org/pandas-docs/stable/indexing.html#indexing-view-versus-copy
      self.obj[item] = s



```python
mapping = agree.merge(kappa, left_index = True, right_index=True).merge(n_chapter, left_index=True, right_index=True)
```


```python
# Display Mapping Reliability Results
mapping.style.format({'agreement_perc': "{:.2%}",
                      'kappa': "{:.3f}"})
```




<style  type="text/css" >
</style>  
<table id="T_85a67b38_cd07_11ea_84ec_4c32759881fb" > 
<thead>    <tr> 
        <th class="blank level0" ></th> 
        <th class="col_heading level0 col0" >agreement_perc</th> 
        <th class="col_heading level0 col1" >kappa</th> 
        <th class="col_heading level0 col2" >n</th> 
    </tr>    <tr> 
        <th class="index_name level0" >kapitelbezeichnung</th> 
        <th class="blank" ></th> 
        <th class="blank" ></th> 
        <th class="blank" ></th> 
    </tr></thead> 
<tbody>    <tr> 
        <th id="T_85a67b38_cd07_11ea_84ec_4c32759881fblevel0_row0" class="row_heading level0 row0" >01 Stammdaten</th> 
        <td id="T_85a67b38_cd07_11ea_84ec_4c32759881fbrow0_col0" class="data row0 col0" >60.78%</td> 
        <td id="T_85a67b38_cd07_11ea_84ec_4c32759881fbrow0_col1" class="data row0 col1" >0.575</td> 
        <td id="T_85a67b38_cd07_11ea_84ec_4c32759881fbrow0_col2" class="data row0 col2" >34</td> 
    </tr>    <tr> 
        <th id="T_85a67b38_cd07_11ea_84ec_4c32759881fblevel0_row1" class="row_heading level0 row1" >02 Allgemeinstatus</th> 
        <td id="T_85a67b38_cd07_11ea_84ec_4c32759881fbrow1_col0" class="data row1 col0" >75.76%</td> 
        <td id="T_85a67b38_cd07_11ea_84ec_4c32759881fbrow1_col1" class="data row1 col1" >0.754</td> 
        <td id="T_85a67b38_cd07_11ea_84ec_4c32759881fbrow1_col2" class="data row1 col2" >66</td> 
    </tr>    <tr> 
        <th id="T_85a67b38_cd07_11ea_84ec_4c32759881fblevel0_row2" class="row_heading level0 row2" >03 Wundanamnese</th> 
        <td id="T_85a67b38_cd07_11ea_84ec_4c32759881fbrow2_col0" class="data row2 col0" >58.33%</td> 
        <td id="T_85a67b38_cd07_11ea_84ec_4c32759881fbrow2_col1" class="data row2 col1" >0.568</td> 
        <td id="T_85a67b38_cd07_11ea_84ec_4c32759881fbrow2_col2" class="data row2 col2" >24</td> 
    </tr>    <tr> 
        <th id="T_85a67b38_cd07_11ea_84ec_4c32759881fblevel0_row3" class="row_heading level0 row3" >04 Wundstatus</th> 
        <td id="T_85a67b38_cd07_11ea_84ec_4c32759881fbrow3_col0" class="data row3 col0" >38.60%</td> 
        <td id="T_85a67b38_cd07_11ea_84ec_4c32759881fbrow3_col1" class="data row3 col1" >0.366</td> 
        <td id="T_85a67b38_cd07_11ea_84ec_4c32759881fbrow3_col2" class="data row3 col2" >57</td> 
    </tr>    <tr> 
        <th id="T_85a67b38_cd07_11ea_84ec_4c32759881fblevel0_row4" class="row_heading level0 row4" >05 Diagnostik</th> 
        <td id="T_85a67b38_cd07_11ea_84ec_4c32759881fbrow4_col0" class="data row4 col0" >33.33%</td> 
        <td id="T_85a67b38_cd07_11ea_84ec_4c32759881fbrow4_col1" class="data row4 col1" >0.280</td> 
        <td id="T_85a67b38_cd07_11ea_84ec_4c32759881fbrow4_col2" class="data row4 col2" >14</td> 
    </tr>    <tr> 
        <th id="T_85a67b38_cd07_11ea_84ec_4c32759881fblevel0_row5" class="row_heading level0 row5" >06 Therapie</th> 
        <td id="T_85a67b38_cd07_11ea_84ec_4c32759881fbrow5_col0" class="data row5 col0" >39.73%</td> 
        <td id="T_85a67b38_cd07_11ea_84ec_4c32759881fbrow5_col1" class="data row5 col1" >0.367</td> 
        <td id="T_85a67b38_cd07_11ea_84ec_4c32759881fbrow5_col2" class="data row5 col2" >73</td> 
    </tr>    <tr> 
        <th id="T_85a67b38_cd07_11ea_84ec_4c32759881fblevel0_row6" class="row_heading level0 row6" >Overall</th> 
        <td id="T_85a67b38_cd07_11ea_84ec_4c32759881fbrow6_col0" class="data row6 col0" >52.36%</td> 
        <td id="T_85a67b38_cd07_11ea_84ec_4c32759881fbrow6_col1" class="data row6 col1" >0.512</td> 
        <td id="T_85a67b38_cd07_11ea_84ec_4c32759881fbrow6_col2" class="data row6 col2" >268</td> 
    </tr></tbody> 
</table> 



**Table 1:** Results of the Mapping, the table presents the proportional agreement for three mapper in the first column and the Fleiss-Kappa Reliability statistic in the second. The third column presents the total number of items in that section of the NKDUC. The last row represents the overall agreement, kappa and total number of items that were mapped in this project. 

# Analysis of the Equivalence Rating

## Fleiss-Kappa Statistic


```python
# Overall Kappa
df1 = res.loc[:, ['equ_jens', 'equ_mareike']].reset_index()
df2 = df1.melt(id_vars='index')
df3 = df2.pivot_table(values='variable', columns='value', aggfunc='count', index='index', fill_value=0)
kappa = {'Overall': fleiss_kappa(df3)}

# Kappa by NKDUC Chapter
for chapter in res.kapitelbezeichnung.drop_duplicates().values:
    df1 = res.loc[res.kapitelbezeichnung == chapter, ['equ_jens', 'equ_mareike']].reset_index()
    df2 = df1.melt(id_vars='index')
    df3 = df2.pivot_table(values='variable', columns='value', aggfunc='count', index='index', fill_value=0)
    kappa[chapter] = fleiss_kappa(df3)

kappa = pd.DataFrame(kappa, index=[0]).transpose()
kappa.columns = ['kappa']
```

## Agreement


```python
agreement_chapter = res.loc[:, ['kapitelbezeichnung', 'agreement']].groupby('kapitelbezeichnung').apply(np.mean)
agreement_overall = res.loc[:, ['agreement']].apply(np.mean)
agreement_overall = pd.DataFrame({'Overall': agreement_overall}).transpose()
agreement = agreement_overall.append(agreement_chapter)
```


```python
# Merge kappa and agreement values
equivalence = pd.merge(kappa, agreement, left_index=True, right_index=True).merge(n_chapter, left_index=True, right_index=True)
equivalence = equivalence.sort_index()
equivalence.loc[:, 'n'] = equivalence.n * 3
```


```python
# Display Equivalence Rating Results
equivalence.loc[:, ['agreement', 'kappa', 'n']].style.format({'agreement': "{:.2%}", 'kappa': "{:.3f}"})
```




<style  type="text/css" >
</style>  
<table id="T_85bee8b2_cd07_11ea_84ec_4c32759881fb" > 
<thead>    <tr> 
        <th class="blank level0" ></th> 
        <th class="col_heading level0 col0" >agreement</th> 
        <th class="col_heading level0 col1" >kappa</th> 
        <th class="col_heading level0 col2" >n</th> 
    </tr></thead> 
<tbody>    <tr> 
        <th id="T_85bee8b2_cd07_11ea_84ec_4c32759881fblevel0_row0" class="row_heading level0 row0" >01 Stammdaten</th> 
        <td id="T_85bee8b2_cd07_11ea_84ec_4c32759881fbrow0_col0" class="data row0 col0" >83.33%</td> 
        <td id="T_85bee8b2_cd07_11ea_84ec_4c32759881fbrow0_col1" class="data row0 col1" >0.772</td> 
        <td id="T_85bee8b2_cd07_11ea_84ec_4c32759881fbrow0_col2" class="data row0 col2" >102</td> 
    </tr>    <tr> 
        <th id="T_85bee8b2_cd07_11ea_84ec_4c32759881fblevel0_row1" class="row_heading level0 row1" >02 Allgemeinstatus</th> 
        <td id="T_85bee8b2_cd07_11ea_84ec_4c32759881fbrow1_col0" class="data row1 col0" >89.90%</td> 
        <td id="T_85bee8b2_cd07_11ea_84ec_4c32759881fbrow1_col1" class="data row1 col1" >0.835</td> 
        <td id="T_85bee8b2_cd07_11ea_84ec_4c32759881fbrow1_col2" class="data row1 col2" >198</td> 
    </tr>    <tr> 
        <th id="T_85bee8b2_cd07_11ea_84ec_4c32759881fblevel0_row2" class="row_heading level0 row2" >03 Wundanamnese</th> 
        <td id="T_85bee8b2_cd07_11ea_84ec_4c32759881fbrow2_col0" class="data row2 col0" >70.83%</td> 
        <td id="T_85bee8b2_cd07_11ea_84ec_4c32759881fbrow2_col1" class="data row2 col1" >0.583</td> 
        <td id="T_85bee8b2_cd07_11ea_84ec_4c32759881fbrow2_col2" class="data row2 col2" >72</td> 
    </tr>    <tr> 
        <th id="T_85bee8b2_cd07_11ea_84ec_4c32759881fblevel0_row3" class="row_heading level0 row3" >04 Wundstatus</th> 
        <td id="T_85bee8b2_cd07_11ea_84ec_4c32759881fbrow3_col0" class="data row3 col0" >74.27%</td> 
        <td id="T_85bee8b2_cd07_11ea_84ec_4c32759881fbrow3_col1" class="data row3 col1" >0.641</td> 
        <td id="T_85bee8b2_cd07_11ea_84ec_4c32759881fbrow3_col2" class="data row3 col2" >171</td> 
    </tr>    <tr> 
        <th id="T_85bee8b2_cd07_11ea_84ec_4c32759881fblevel0_row4" class="row_heading level0 row4" >05 Diagnostik</th> 
        <td id="T_85bee8b2_cd07_11ea_84ec_4c32759881fbrow4_col0" class="data row4 col0" >59.52%</td> 
        <td id="T_85bee8b2_cd07_11ea_84ec_4c32759881fbrow4_col1" class="data row4 col1" >0.408</td> 
        <td id="T_85bee8b2_cd07_11ea_84ec_4c32759881fbrow4_col2" class="data row4 col2" >42</td> 
    </tr>    <tr> 
        <th id="T_85bee8b2_cd07_11ea_84ec_4c32759881fblevel0_row5" class="row_heading level0 row5" >06 Therapie</th> 
        <td id="T_85bee8b2_cd07_11ea_84ec_4c32759881fbrow5_col0" class="data row5 col0" >75.34%</td> 
        <td id="T_85bee8b2_cd07_11ea_84ec_4c32759881fbrow5_col1" class="data row5 col1" >0.667</td> 
        <td id="T_85bee8b2_cd07_11ea_84ec_4c32759881fbrow5_col2" class="data row5 col2" >219</td> 
    </tr>    <tr> 
        <th id="T_85bee8b2_cd07_11ea_84ec_4c32759881fblevel0_row6" class="row_heading level0 row6" >Overall</th> 
        <td id="T_85bee8b2_cd07_11ea_84ec_4c32759881fbrow6_col0" class="data row6 col0" >78.48%</td> 
        <td id="T_85bee8b2_cd07_11ea_84ec_4c32759881fbrow6_col1" class="data row6 col1" >0.702</td> 
        <td id="T_85bee8b2_cd07_11ea_84ec_4c32759881fbrow6_col2" class="data row6 col2" >804</td> 
    </tr></tbody> 
</table> 



**Table 2:** *Reliability of the Equivalence Rating between two Rater.* To evaluate the coverage rate and the quality of the Mapping, two persons conducted an equivalence rating as it is described in the ISO Technical Report 12300. During this task, the two persons independently rated the semantic equivalence of the source NKDUC items and the target SNOMED CT concepts.

# Coverage Rate


```python
all(res.loc[:, ['id','finales_konzept']].groupby('id').count().finales_konzept == 3)
```




    False




```python
concept_ids = res.drop_duplicates(['id']).loc[:, ['id']]
df=res.loc[res.finales_konzept==1, ['id','snomed_code', 'finaler_beschluss', 'descriptor','finales_konzept']]
df.head()
df.groupby('id').apply(np.mean).shape[0]
res.loc[:, ['id', 'map']].drop_duplicates().shape
```




    (268, 2)




```python
# Create a dataset that contain the final concepts
final_map=res.loc[res.finales_konzept==1, ['id','snomed_code', 'descriptor', 'equi_final']].drop_duplicates()
final_map=res.drop_duplicates('id').loc[:, ['id', 'kapitelbezeichnung', 'finaler_beschluss']].merge(final_map, on='id', how='left')
final_map.loc[:, 'descriptor'] = final_map['descriptor'].fillna('no map')
final_map.loc[:, 'snomed_code'] = final_map['snomed_code'].fillna('no map')
final_map.loc[:, 'equi_final'] = final_map['equi_final'].fillna('No map is possible')
final_map['snomed_code'] = final_map['snomed_code'].apply(lambda x: False if x == 'no map' else True)

```


```python
final_map.pivot_table(index='kapitelbezeichnung', values='snomed_code', aggfunc=('count', 'mean')).style.format({'mean': "{:.2%}"})
```




<style  type="text/css" >
</style>  
<table id="T_85f9f60a_cd07_11ea_84ec_4c32759881fb" > 
<thead>    <tr> 
        <th class="blank level0" ></th> 
        <th class="col_heading level0 col0" >count</th> 
        <th class="col_heading level0 col1" >mean</th> 
    </tr>    <tr> 
        <th class="index_name level0" >kapitelbezeichnung</th> 
        <th class="blank" ></th> 
        <th class="blank" ></th> 
    </tr></thead> 
<tbody>    <tr> 
        <th id="T_85f9f60a_cd07_11ea_84ec_4c32759881fblevel0_row0" class="row_heading level0 row0" >01 Stammdaten</th> 
        <td id="T_85f9f60a_cd07_11ea_84ec_4c32759881fbrow0_col0" class="data row0 col0" >34</td> 
        <td id="T_85f9f60a_cd07_11ea_84ec_4c32759881fbrow0_col1" class="data row0 col1" >64.71%</td> 
    </tr>    <tr> 
        <th id="T_85f9f60a_cd07_11ea_84ec_4c32759881fblevel0_row1" class="row_heading level0 row1" >02 Allgemeinstatus</th> 
        <td id="T_85f9f60a_cd07_11ea_84ec_4c32759881fbrow1_col0" class="data row1 col0" >66</td> 
        <td id="T_85f9f60a_cd07_11ea_84ec_4c32759881fbrow1_col1" class="data row1 col1" >89.39%</td> 
    </tr>    <tr> 
        <th id="T_85f9f60a_cd07_11ea_84ec_4c32759881fblevel0_row2" class="row_heading level0 row2" >03 Wundanamnese</th> 
        <td id="T_85f9f60a_cd07_11ea_84ec_4c32759881fbrow2_col0" class="data row2 col0" >24</td> 
        <td id="T_85f9f60a_cd07_11ea_84ec_4c32759881fbrow2_col1" class="data row2 col1" >83.33%</td> 
    </tr>    <tr> 
        <th id="T_85f9f60a_cd07_11ea_84ec_4c32759881fblevel0_row3" class="row_heading level0 row3" >04 Wundstatus</th> 
        <td id="T_85f9f60a_cd07_11ea_84ec_4c32759881fbrow3_col0" class="data row3 col0" >57</td> 
        <td id="T_85f9f60a_cd07_11ea_84ec_4c32759881fbrow3_col1" class="data row3 col1" >78.95%</td> 
    </tr>    <tr> 
        <th id="T_85f9f60a_cd07_11ea_84ec_4c32759881fblevel0_row4" class="row_heading level0 row4" >05 Diagnostik</th> 
        <td id="T_85f9f60a_cd07_11ea_84ec_4c32759881fbrow4_col0" class="data row4 col0" >14</td> 
        <td id="T_85f9f60a_cd07_11ea_84ec_4c32759881fbrow4_col1" class="data row4 col1" >78.57%</td> 
    </tr>    <tr> 
        <th id="T_85f9f60a_cd07_11ea_84ec_4c32759881fblevel0_row5" class="row_heading level0 row5" >06 Therapie</th> 
        <td id="T_85f9f60a_cd07_11ea_84ec_4c32759881fbrow5_col0" class="data row5 col0" >73</td> 
        <td id="T_85f9f60a_cd07_11ea_84ec_4c32759881fbrow5_col1" class="data row5 col1" >78.08%</td> 
    </tr></tbody> 
</table> 



**The table above is not part of the publication, it is a quick overview of the coverage rate, i.e., how many concepts could be matches, regardless of the equality category of the match itself.**


```python
cont_table = final_map.pivot_table(index='equi_final', values='snomed_code', columns='kapitelbezeichnung', aggfunc='count')
```


```python
assert np.array(cont_table.fillna(0)).sum()==268 # Check that 268 items are included in the coverage rate analysis
```


```python
# Count number of items (equality categories) in each chapter and each categories
df1 = final_map.pivot_table(index=['kapitelbezeichnung', 'equi_final'], values='snomed_code', aggfunc='count').reset_index()
```


```python
# Add the count of items in each chapter so that the proportion of matches can be computed for
# each chapter and each matching category
# e.g., 12 items in 01-Allgemeinstatus had a match of category 5: no map is possible
df2=df1.merge(n_chapter, right_index=True, left_on='kapitelbezeichnung', how='left')
# This line actually computes the percentage/ coverage rate dependent of the chapter + category (see above)
df2.loc[:, 'coverage_perc']=df2.snomed_code / df2.n

# Pretty print
# Add number of total items and the relative items
l = list()
for i in range(df2.shape[0]):
    l.append("{:.1%} (n={})".format(df2.coverage_perc[i], df2.snomed_code[i]))
# Append the computes values as new column to pd.dataframe
df2.loc[:, 'coverage'] = pd.Series(l)
```


```python
assert all(df2.groupby('kapitelbezeichnung').coverage_perc.sum().values == 1) # check if perc adds up to 1 (100%)
```


```python
# Actually format for display the data (pivot_wide: Chapters in cols and equi categories in the rows)
df3=df2.pivot(index='equi_final', columns='kapitelbezeichnung', values='coverage').fillna("-").reset_index()
df3.loc[:, 'equi_final'] = df3.equi_final.astype('category')
df3.loc[:, 'equi_final'] = df3['equi_final'].cat.reorder_categories(list(iso_categories.keys()))
df3 = df3.sort_values(by='equi_final')
df3.set_index('equi_final', inplace=True)
# Display Detailled Coverage Rate
display(df3)
```


<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th>kapitelbezeichnung</th>
      <th>01 Stammdaten</th>
      <th>02 Allgemeinstatus</th>
      <th>03 Wundanamnese</th>
      <th>04 Wundstatus</th>
      <th>05 Diagnostik</th>
      <th>06 Therapie</th>
    </tr>
    <tr>
      <th>equi_final</th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>Equivalence of meaning; lexical, as well as conceptual</th>
      <td>23.5% (n=8)</td>
      <td>59.1% (n=39)</td>
      <td>50.0% (n=12)</td>
      <td>43.9% (n=25)</td>
      <td>35.7% (n=5)</td>
      <td>38.4% (n=28)</td>
    </tr>
    <tr>
      <th>Equivalence of meaning, but with synonymy.</th>
      <td>26.5% (n=9)</td>
      <td>24.2% (n=16)</td>
      <td>25.0% (n=6)</td>
      <td>21.1% (n=12)</td>
      <td>21.4% (n=3)</td>
      <td>23.3% (n=17)</td>
    </tr>
    <tr>
      <th>Source concept is broader and has a less specific meaning than the target concept</th>
      <td>2.9% (n=1)</td>
      <td>3.0% (n=2)</td>
      <td>4.2% (n=1)</td>
      <td>1.8% (n=1)</td>
      <td>-</td>
      <td>1.4% (n=1)</td>
    </tr>
    <tr>
      <th>Source concept is narrower and has a more specific meaning than the target concept</th>
      <td>11.8% (n=4)</td>
      <td>3.0% (n=2)</td>
      <td>-</td>
      <td>10.5% (n=6)</td>
      <td>21.4% (n=3)</td>
      <td>15.1% (n=11)</td>
    </tr>
    <tr>
      <th>No map is possible</th>
      <td>35.3% (n=12)</td>
      <td>10.6% (n=7)</td>
      <td>20.8% (n=5)</td>
      <td>22.8% (n=13)</td>
      <td>21.4% (n=3)</td>
      <td>21.9% (n=16)</td>
    </tr>
  </tbody>
</table>
</div>


**Appendix 1:** The table shows the coverage rates for each category for each section of the NKDUC. In the publication we included the table with aggregated categories, to demonstrate the number of matches (Cat 1 + 2), matches with asymmetry (Cat 3 + 4), and no matches (Cat 5).

*This aggregated table is shown below*


```python
#df2.loc[:, 'equi_final'] = df2.equi_final.astype('category', categories=list(d.keys()), ordered=True)
iso_cats_agg = {
    'Equivalence of meaning; lexical, as well as conceptual': 'Semantic Match present (Degree 1 and 2)',
    'Equivalence of meaning, but with synonymy.': 'Semantic Match present (Degree 1 and 2)',
    'Source concept is broader and has a less specific meaning than the target concept': 'Semantic Asymmetry present (Degree 3 and 4)',
    'Source concept is narrower and has a more specific meaning than the target concept': 'Semantic Asymmetry present (Degree 3 and 4)',
    'No map is possible': 'Semantic Match absent (Degree 5)', 
}

```


```python
df3.index = df3.index.map(iso_cats_agg)
```


```python
# Define the category order for the categorical variable 'Final equivalence rating (equi_final)'
cats = ['Semantic Match present (Degree 1 and 2)', 'Semantic Asymmetry present (Degree 3 and 4)', 'Semantic Match absent (Degree 5)']
```


```python
coverage = df2.copy(deep=True)
coverage.loc[:, 'equi_final'] = coverage.equi_final.map(iso_cats_agg)

coverage.drop('n', axis=1, inplace=True)
coverage.rename({'snomed_code': 'n'}, axis=1, inplace=True)

coverage = coverage.groupby(['kapitelbezeichnung', 'equi_final']).agg({'n': np.sum, 'coverage_perc': np.sum})

def format_aggregates(data):
    l = list()
    for i in range(data.shape[0]):
        l.append("{:.1%} (n={})".format(data.coverage_perc[i], data.n[i]))
    data.loc[:, 'formatted'] = l
    return(data)

format_aggregates(coverage).reset_index(inplace=True)

coverage.loc[:, 'equi_final'] = coverage.equi_final.astype('category')
coverage.loc[:, 'equi_final'] = coverage.equi_final.cat.reorder_categories(new_categories=cats, ordered=True)

coverage_chapters = coverage.pivot(index='equi_final', columns='kapitelbezeichnung', values='formatted')

coverage_overall = coverage.set_index('equi_final').groupby(level=0).agg({'n': np.sum})
coverage_overall = coverage_overall.assign(coverage_perc=lambda x: x.n/268)
coverage_overall = coverage_overall.assign(formatted=lambda x: format_aggregates(x).formatted)
coverage_overall.rename({'formatted': 'Overall'}, axis=1, inplace=True)

coverage_overall.loc[:,['Overall']].merge(coverage_chapters, left_index=True, right_index=True, how='left')

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>Overall</th>
      <th>01 Stammdaten</th>
      <th>02 Allgemeinstatus</th>
      <th>03 Wundanamnese</th>
      <th>04 Wundstatus</th>
      <th>05 Diagnostik</th>
      <th>06 Therapie</th>
    </tr>
    <tr>
      <th>equi_final</th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
      <th></th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>Semantic Match present (Degree 1 and 2)</th>
      <td>67.2% (n=180)</td>
      <td>50.0% (n=17)</td>
      <td>83.3% (n=55)</td>
      <td>75.0% (n=18)</td>
      <td>64.9% (n=37)</td>
      <td>57.1% (n=8)</td>
      <td>61.6% (n=45)</td>
    </tr>
    <tr>
      <th>Semantic Asymmetry present (Degree 3 and 4)</th>
      <td>11.9% (n=32)</td>
      <td>14.7% (n=5)</td>
      <td>6.1% (n=4)</td>
      <td>4.2% (n=1)</td>
      <td>12.3% (n=7)</td>
      <td>21.4% (n=3)</td>
      <td>16.4% (n=12)</td>
    </tr>
    <tr>
      <th>Semantic Match absent (Degree 5)</th>
      <td>20.9% (n=56)</td>
      <td>35.3% (n=12)</td>
      <td>10.6% (n=7)</td>
      <td>20.8% (n=5)</td>
      <td>22.8% (n=13)</td>
      <td>21.4% (n=3)</td>
      <td>21.9% (n=16)</td>
    </tr>
  </tbody>
</table>
</div>



**Table 3:** Coverage rate of the mapping presented using ISO TR 12300 equivalence categories for the complete NKDUC and each of its sections.
