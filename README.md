<a name="BackToTop"></a>

# Yelp_Hotels_Topic_Modeling_and_ANOVA

>For this project, I used the Yelp Academic Dataset to obtain reviews about hotels. After cleaning the data, I clustered the reviews using structural topic modeling. Afterwards, I was interested in the idea that airbnb's increasing impact on the market could correlate with a change in ratings. With the topics determined from the topic modeling section, I performed an ANOVA to test this hypothesis.


---


## Table of Contents
- [Data Exploration Summary](#Data_Exploration)
- [Topic Modeling Summary](#Topic_Modeling)
- [ANOVA Summary](#ANOVA)
- [References and Resources](#References_and_Resources)
- [How to Use](#How_to_Use)


---


<a name="Data_Exploration"></a>  

## Data Exploration Summary
Out of the original 8 million reviews, about 250,000 were for hotels. Each business is classified with a series of categories, so I first removed any business that did not fall under the "Hotel" category. Afterwards, I removed any that were not actually hotels despite falling under this category. This included restaurants and bars that were associated with a hotel, as well as removing bus tours and reviews that were booked via Airbnb.  

Below are a series of charts found in the data exploration section. Please note, many charts are from a random sample of the total hotel reviews. The third chart involves Flesch-Kincaid scores, which measures how easily a passage is understood.    

After removing extremely common words, aka stop words, I created word clouds for the most common words and mapped words that commonly appeared together.   

[Back to Top](#BackToTop)


---

<a name="Topic_Modeling"></a>

## Topic Modeling Summary


[Back to Top](#BackToTop)


---

<a name="ANOVA"></a>

## ANOVA Summary


[Back to Top](#BackToTop)


---


<a name="References_and_Resources"></a>

## References and Resources

https://github.com/wesslen/text-analysis-org-science provides a well annotated and thorough explanation of the structural topic modeling steps. I also used https://juliasilge.com/blog/evaluating-stm/

[Back to Top](#BackToTop)


---


<a name="How to Use"></a>

## How to Use
The dataset that was used can be accesed at https://www.yelp.com/dataset  
There is also a similar dataset located on Kaggle. The businesses that are included are located in Montreal, Calgary, Toronto, Pittsburgh, Charlotte, Urbana-Champaign, Phoenix, Las Vegas, Madison, and Cleveland. To import the Yelp Reviews, my personal computer did not have enough RAM, and I needed access to my University's research computer.

##### Technologies
R Studio  
R version 4.0.3

[Back to Top](#BackToTop)