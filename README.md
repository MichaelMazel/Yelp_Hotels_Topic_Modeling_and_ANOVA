<a name="BackToTop"></a>

# Yelp_Hotels_Topic_Modeling_and_ANOVA

>For this project, I used the Yelp Academic Dataset to obtain reviews about hotels. After cleaning the data, I clustered the reviews using structural topic modeling. Afterwards, I was interested in the idea that airbnb's increasing impact on the market could correlate with a change in ratings. With the topics determined from the topic modeling section, I performed an ANOVA to test this hypothesis. The full code can be found above in both an .rmd and .md format or continue reading for a summary.


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

Below are a series of charts found in the data exploration section. Please note, many charts are from a random sample of the total hotel reviews. The second chart involves Flesch-Kincaid scores, which measures how easily a passage is understood.   
![Words_Per_Hotel_Review](https://github.com/MichaelMazel/Yelp_Hotels_Topic_Modeling_and_ANOVA/blob/main/Yelp_Hotels_Topic_Modeling_and_ANOVA_files/figure-gfm/unnamed-chunk-13-2.png)  
![Flesch_Kincaid_Scores](https://github.com/MichaelMazel/Yelp_Hotels_Topic_Modeling_and_ANOVA/blob/main/Yelp_Hotels_Topic_Modeling_and_ANOVA_files/figure-gfm/unnamed-chunk-13-1.png)   

After removing extremely common words, aka stop words, I created word clouds for the most common words and mapped words that commonly appeared together.  
![Word_Cloud](https://github.com/MichaelMazel/Yelp_Hotels_Topic_Modeling_and_ANOVA/blob/main/Yelp_Hotels_Topic_Modeling_and_ANOVA_files/figure-gfm/unnamed-chunk-21-1.png)  
![City_Word_Cloud](https://github.com/MichaelMazel/Yelp_Hotels_Topic_Modeling_and_ANOVA/blob/main/Yelp_Hotels_Topic_Modeling_and_ANOVA_files/figure-gfm/unnamed-chunk-22-1.png)  
![Words_Appearing_Together](https://github.com/MichaelMazel/Yelp_Hotels_Topic_Modeling_and_ANOVA/blob/main/Yelp_Hotels_Topic_Modeling_and_ANOVA_files/figure-gfm/unnamed-chunk-24-1.png)  

[Back to Top](#BackToTop)


---

<a name="Topic_Modeling"></a>

## Topic Modeling Summary  
I performed structural topic modeling with and without covariates, but I will only discuss with covariates in this section. I believed that hotel experiences/expectations would likely be impacted by the city, and thus used this as a covariate. In fact, two-thirds of all hotel reviews in this dataset were in Las Vegas.  

The plot below shows the reviews clustered together according to different number of topics. The held-out likelihood is essentially how well a model performs on unseen documents. Semantic coherence is high when a collection of words commonly appear together in a given review. Based off these criteria, I found 10 to be the optimal number of topics.

![Number_of_Topics](https://github.com/MichaelMazel/Yelp_Hotels_Topic_Modeling_and_ANOVA/blob/main/Yelp_Hotels_Topic_Modeling_and_ANOVA_files/figure-gfm/unnamed-chunk-43-1.png)  

I renamed the ten topics in an attempt to best summarize each topic's top words. They include "Transactions", "Events", "Vegas Casinos", "Convenience", "Discrepancies", "Las Vegas", "Pleasant Experience", "Room Features", "Food","Complaints". Below is a comparison of two seemingly similar topics, Discrepancies and Complaints:  

![Comparison_of_Topics](https://github.com/MichaelMazel/Yelp_Hotels_Topic_Modeling_and_ANOVA/blob/main/Yelp_Hotels_Topic_Modeling_and_ANOVA_files/figure-gfm/unnamed-chunk-51-1.png)  

One benefit of structural topic modeling compared to Latent Dirichlet Allocation (LDA) is the ability to analyze how topics correlate one another. Below is a visual representation.  

![Topic_Correlations](https://github.com/MichaelMazel/Yelp_Hotels_Topic_Modeling_and_ANOVA/blob/main/Yelp_Hotels_Topic_Modeling_and_ANOVA_files/figure-gfm/unnamed-chunk-51-1.png)  


[Back to Top](#BackToTop)


---

<a name="ANOVA"></a>

## ANOVA Summary

Now that we created the topics, I hypothesized that certain topics would have a change in stars (scale 1-5) that correlate to Airbnb's growing influence in the market. Some topics like "complaints", I did not expect a change, while others like "room features" I did. My theory was that there could be higher expectations for hotels now that Airbnb is offering a unique and potentially refreshing alternative. Alternatively, users may be appreciating hotels more due to some of their competitive advantages.  

Below is the number of Yelp reviews that mention Airbnb each year. I used this as one of the methods to group reviews into time periods based off the Technology Adoption Life Cycle Theory. The groups include "Early Adopters" (which also include pre-Airbnb), "Early Majority","Late Majority", or "Laggards". After the chart of Airbnb reviews by year, is the ratings distribution by each time period.      

![Airbnb_Mentions](https://github.com/MichaelMazel/Yelp_Hotels_Topic_Modeling_and_ANOVA/blob/main/Yelp_Hotels_Topic_Modeling_and_ANOVA_files/figure-gfm/unnamed-chunk-57-1.png)   
![Ratings_by_Time_Period](https://github.com/MichaelMazel/Yelp_Hotels_Topic_Modeling_and_ANOVA/blob/main/Yelp_Hotels_Topic_Modeling_and_ANOVA_files/figure-gfm/unnamed-chunk-59-1.png)   

For topics of interest, I performed an ANOVA to test for a difference across any of the four time periods. If it returned significant, I performed a multiple comparisons test (adjusted with Bonferroni). Finally, I calculated Cohen's D to see if the effect size was meaningful.  

The Transactions Topic included key words such as fee, internet, car, charges, checkout, and cost.
Early Adopters in this topic left a review that was 0.61 stars higher on average compared to Laggards. The Early Adopters included reviews from 2014 and before, while Laggards included those from 2018 and after. The .61 difference in stars translates to a 0.48 Cohen's D, implying moderate effect size.

The Convenience Topic included words such as breakfast, shuttle, location, staff, and lobby. Again, Early Adopters left a review significantly higher than Laggards. This difference was 0.37 stars which translates to a much smaller 0.27 Cohen's D.  

The Room Features Topic included words such as tv, cable, couch, ice, hallway, mini, and mirror. The Early Adopters group were significantly higher than all the other time periods. The difference was actually the highest between Early Adopters and Late Majority, not Laggards. This translated to a 0.45 Cohen's D.   

The Vegas Casinos Topic included key words such as strip, casino, slots, poker, and fremont. None of the time periods returned average stars that were significantly different than another time period.  


Limitations:
-From the histograms of rating scores by time period, it appears there could be a general shift in how users express their thoughts. It is likely users leave more polarizing reviews now compared to 5 or 10 years ago. Future research to address this could be performing a sentiment analysis, and while holding sentiment constant, test if ratings change over time.  
-Other competitors besides Airbnb that could have an impact on the market include Expedia Group, Tripadvisor, and Bookings Holdings.    
-I used city as a covariate, but because Vegas has such a large proportion of total reviews, the topics could still have a bias.   


[Back to Top](#BackToTop)


---


<a name="References_and_Resources"></a>

## References and Resources  

https://github.com/wesslen/text-analysis-org-science provides a well annotated and thorough explanation of the structural topic modeling steps. I also used https://juliasilge.com/blog/evaluating-stm/  

This paper provided good insight on advantages of structural topic modeling: https://scholar.harvard.edu/files/dtingley/files/topicmodelsopenendedexperiments.pdf 

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