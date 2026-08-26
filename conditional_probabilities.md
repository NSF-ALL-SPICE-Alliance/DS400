## Bayes Theorem & Conditional Probabilities Workshop


### Data
[Kaggle Stroke Prediction Dataset](https://www.kaggle.com/datasets/fedesoriano/stroke-prediction-dataset/data)


### Three Teams based on variable
1. Gender
2. Hypertension
3. Heart Disease

### Exploratory Data Analysis
- Download the data 
- Open an new QMD and name it conditional_probability_workshop
- Read the data in with `read_csv()` and `here()`
- Write R Code to create these 4 visualizations with `ggplot`



<img width="2228" height="1332" alt="image" src="https://github.com/user-attachments/assets/45c7db8b-476f-42f9-96bc-0e5886087ab5" />

<img width="2228" height="1332" alt="image" src="https://github.com/user-attachments/assets/5faba5e0-4526-4385-b975-dda6779564bb" />

<img width="2228" height="1332" alt="image" src="https://github.com/user-attachments/assets/e927bf47-8cd0-4cfb-8805-8dbd2c10dd5b" />

<img width="2228" height="1332" alt="image" src="https://github.com/user-attachments/assets/351c87cc-c5a9-484e-9df5-c779955b5da7" />

<img width="2228" height="1332" alt="image" src="https://github.com/user-attachments/assets/ece38b80-72ea-423d-8daf-b0d0e84d0f06" />





### Find the Conditional Probability of Stroke P(B|A) based on 
1. Being a Male
2. Having Hypertension
3. Having Heart Disease

Use this method from the last class:

##### P(B)

```{r}
patients %>% 
  count(has_disease) %>% 
  mutate(percent = n / sum(n))
```

```{r}
probability_disease <- patients %>% 
  count(has_disease) %>% 
  mutate(percent = n / sum(n)) %>% 
  filter(has_disease == 1) %>% 
  pull(percent)
```

##### P(A|B)

```{r}
patients %>% 
  count(has_disease, test_result) %>% 
  group_by(has_disease) %>% 
  mutate(percent = n / sum(n))
```

```{r}
probability_positive_result_given_disease <- patients %>% 
  count(has_disease, test_result) %>% 
  group_by(has_disease) %>% 
  mutate(percent = n / sum(n)) %>% 
  filter(has_disease == 1, test_result == "positive") %>% 
  pull(percent)
```

##### P(A)

```{r}
patients %>% 
  count(test_result) %>% 
  mutate(percent = n / sum(n))
```

```{r}
probability_positive_test_result <- patients %>% 
  count(test_result) %>% 
  mutate(percent = n / sum(n)) %>% 
  filter(test_result == "positive") %>% 
  pull(percent)
```

##### P(B|A)

```{r}
(probability_positive_result_given_disease * probability_disease) / probability_positive_test_result
```

### Report out your conditional probabilities and we will compare


### Discussion
What question are we asking exactly
- [WHO](https://www.who.int/news-room/fact-sheets/detail/stroke)
- [Data Quality](https://link.springer.com/10.1186/s12916-026-04981-y)







