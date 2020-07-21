
Import data


```python
import pandas as pd
from statsmodels.stats.inter_rater import fleiss_kappa
import numpy as np
res = pd.read_csv("/Users/jens/r-projects/proj_packages/snomed/data/mapping-results.csv")

# res.finales_konzept = res.finales_konzept.map({1: True, 1: False}).astype('bool')
```

# Analysis of the SNOMED Mapping


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

## Agreement


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

## Fleiss-Kappa


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
display(mapping)
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
      <th>agreement_perc</th>
      <th>kappa</th>
      <th>n</th>
    </tr>
    <tr>
      <th>kapitelbezeichnung</th>
      <th></th>
      <th></th>
      <th></th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>01 Stammdaten</th>
      <td>0.607843</td>
      <td>0.575000</td>
      <td>34</td>
    </tr>
    <tr>
      <th>02 Allgemeinstatus</th>
      <td>0.757576</td>
      <td>0.753668</td>
      <td>66</td>
    </tr>
    <tr>
      <th>03 Wundanamnese</th>
      <td>0.583333</td>
      <td>0.567827</td>
      <td>24</td>
    </tr>
    <tr>
      <th>04 Wundstatus</th>
      <td>0.385965</td>
      <td>0.365861</td>
      <td>57</td>
    </tr>
    <tr>
      <th>05 Diagnostik</th>
      <td>0.333333</td>
      <td>0.280294</td>
      <td>14</td>
    </tr>
    <tr>
      <th>06 Therapie</th>
      <td>0.397260</td>
      <td>0.367274</td>
      <td>73</td>
    </tr>
    <tr>
      <th>Overall</th>
      <td>0.523632</td>
      <td>0.512113</td>
      <td>268</td>
    </tr>
  </tbody>
</table>
</div>


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
agreement = agreement_chapter.append(agreement_overall)
```


```python
# Merge kappa and agreement values
agreement_kappa = pd.merge(kappa, agreement, left_index=True, right_index=True).merge(n_chapter, left_index=True, right_index=True)
```


```python
# Display Equivalence Rating Results
display(agreement_kappa)
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
      <th>kappa</th>
      <th>agreement</th>
      <th>n</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>Overall</th>
      <td>0.702432</td>
      <td>0.784826</td>
      <td>268</td>
    </tr>
    <tr>
      <th>01 Stammdaten</th>
      <td>0.771557</td>
      <td>0.833333</td>
      <td>34</td>
    </tr>
    <tr>
      <th>02 Allgemeinstatus</th>
      <td>0.834628</td>
      <td>0.898990</td>
      <td>66</td>
    </tr>
    <tr>
      <th>03 Wundanamnese</th>
      <td>0.582551</td>
      <td>0.708333</td>
      <td>24</td>
    </tr>
    <tr>
      <th>04 Wundstatus</th>
      <td>0.640876</td>
      <td>0.742690</td>
      <td>57</td>
    </tr>
    <tr>
      <th>05 Diagnostik</th>
      <td>0.407960</td>
      <td>0.595238</td>
      <td>14</td>
    </tr>
    <tr>
      <th>06 Therapie</th>
      <td>0.666944</td>
      <td>0.753425</td>
      <td>73</td>
    </tr>
  </tbody>
</table>
</div>


# Coverage Rate


```python
all(res.loc[:, ['id','finales_konzept']].groupby('id').count().finales_konzept == 3)
```




    True




```python
concept_ids = res.drop_duplicates(['id']).loc[:, ['id']]
df=res.loc[res.finales_konzept==1, ['id','snomed_code', 'finaler_beschluss', 'descriptor','finales_konzept']]
df.head()
df.groupby('id').apply(np.mean).shape[0]
res.loc[:, ['id', 'map']].drop_duplicates().shape
```




    (268, 2)




```python
final_map=res.loc[res.finales_konzept==1, ['id','snomed_code', 'descriptor', 'equi_final']].drop_duplicates()
final_map=res.drop_duplicates('id').loc[:, ['id', 'kapitelbezeichnung', 'finaler_beschluss']].merge(final_map, on='id', how='left')
final_map.loc[:, 'descriptor'] = final_map['descriptor'].fillna('no map')
final_map.loc[:, 'snomed_code'] = final_map['snomed_code'].fillna('no map')
final_map.loc[:, 'equi_final'] = final_map['equi_final'].fillna('No map is possible')
final_map['snomed_code'] = final_map['snomed_code'].apply(lambda x: False if x == 'no map' else True)
```


```python
final_map.pivot_table(index='kapitelbezeichnung', values='snomed_code', aggfunc=('count', 'mean'))
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
      <th>count</th>
      <th>mean</th>
    </tr>
    <tr>
      <th>kapitelbezeichnung</th>
      <th></th>
      <th></th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>01 Stammdaten</th>
      <td>34</td>
      <td>0.647059</td>
    </tr>
    <tr>
      <th>02 Allgemeinstatus</th>
      <td>66</td>
      <td>0.893939</td>
    </tr>
    <tr>
      <th>03 Wundanamnese</th>
      <td>24</td>
      <td>0.833333</td>
    </tr>
    <tr>
      <th>04 Wundstatus</th>
      <td>57</td>
      <td>0.771930</td>
    </tr>
    <tr>
      <th>05 Diagnostik</th>
      <td>14</td>
      <td>0.785714</td>
    </tr>
    <tr>
      <th>06 Therapie</th>
      <td>73</td>
      <td>0.780822</td>
    </tr>
  </tbody>
</table>
</div>




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



```python
#df2.loc[:, 'equi_final'] = df2.equi_final.astype('category', categories=list(d.keys()), ordered=True)
iso_categories_aggregated = {
    'Equivalence of meaning; lexical, as well as conceptual': 'Semantic Match present (Degree 1 and 2)',
    'Equivalence of meaning, but with synonymy.': 'Semantic Match present (Degree 1 and 2)',
    'Source concept is broader and has a less specific meaning than the target concept': 'Semantic Asymmetry present (Degree 3 and 4)',
    'Source concept is narrower and has a more specific meaning than the target concept': 'Semantic Asymmetry present (Degree 3 and 4)',
    'No map is possible': 'Semantic Match absent (Degree 5)', 
}

```


```python
iso_categories_agg = pd.DataFrame(iso_categories_aggregated, index=[0]).transpose().reset_index()
iso_categories_agg.columns = ['complete', 'aggregated']

equi_cats = df2.merge(iso_categories_agg, left_on='equi_final', right_on='complete', how='left')

equi_cats = equi_cats.loc[:, ['kapitelbezeichnung', 'aggregated', 'snomed_code']]
equi_cats = equi_cats.groupby(['kapitelbezeichnung', 'aggregated']).agg(np.sum)
equi_cats = equi_cats.reset_index().merge(n_chapter, left_on='kapitelbezeichnung', right_index=True)
equi_cats.loc[:, 'coverage_perc'] = equi_cats.snomed_code / equi_cats.n
equi_cats


# Pretty print (relative + absolute value in one column)
for i in range(equi_cats.shape[0]):
    l.append("{:.1%} (n={})".format(equi_cats.coverage_perc[i], equi_cats.snomed_code[i]))
# Append the computes values as new column to pd.dataframe
equi_cats.loc[:, 'coverage'] = pd.Series(l)

category_order = pd.Series(list(iso_categories_aggregated.values())).drop_duplicates().values
equi_cats.loc[:, 'aggregated'] = equi_cats.aggregated.astype('category')
equi_cats.loc[:, 'aggregated'] = equi_cats.aggregated.cat.reorder_categories(new_categories=category_order, ordered=True)
```


```python
equi_cats_pretty = equi_cats.pivot(index='aggregated', values='coverage', columns='kapitelbezeichnung')
# Display aggregated Coverage Rate
display(equi_cats_pretty)
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
      <th>aggregated</th>
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
      <td>35.3% (n=12)</td>
      <td>24.2% (n=16)</td>
      <td>3.0% (n=2)</td>
      <td>50.0% (n=12)</td>
      <td>21.1% (n=12)</td>
      <td>1.8% (n=1)</td>
    </tr>
    <tr>
      <th>Semantic Asymmetry present (Degree 3 and 4)</th>
      <td>26.5% (n=9)</td>
      <td>2.9% (n=1)</td>
      <td>59.1% (n=39)</td>
      <td>3.0% (n=2)</td>
      <td>20.8% (n=5)</td>
      <td>43.9% (n=25)</td>
    </tr>
    <tr>
      <th>Semantic Match absent (Degree 5)</th>
      <td>23.5% (n=8)</td>
      <td>11.8% (n=4)</td>
      <td>10.6% (n=7)</td>
      <td>25.0% (n=6)</td>
      <td>4.2% (n=1)</td>
      <td>22.8% (n=13)</td>
    </tr>
  </tbody>
</table>
</div>

