# Using Data to Power the Grid: A Case Study of a Hydroelectric Utility Company

## Business Problem
Access to electricity is essential as its woven into nearly every aspect of our lives. Consumers use electricity in every second of every day, yet it is rare for people to consider where their power is coming from or how their power is generated. What would happen if consumers had to depend on unreliable energy? There could be a myriad of issues like traffic accidents, lapses in medical care, and a lack of stability in global economies. That's why it's important to ensure that all energy sources are sustainable and reliable. 

My team at Syracuse University was outsourced by a hydroelectric utility company to identify ways to improve operational efficiency at the companies' various hydro plants. The goal was to optimize power outages by predicting the ideal outage schedule that would result in maximizing revenue streams and minimizing costs. After further research, the team discovered many variables affecting the companies ability to supply affordable, reliable, and renewable energy. Variables like low flow of water supply, low load of units of energy, and high rates of forced outages or planned outages lead to either a deficit in energy supplies or a decrease in revenue streams. 

## About the Data 
Four main data sets, containing hourly records from 1999 to 2018, were provided by the hydroelectric utility company in Excel spreadsheet format. The outage data describes the two types of outages and details surrounding their occurrence. The flow and load data both explain variance in flow, the actual input or hydro energy supply, and load, the units needed to generate power. The marketing dataset provides records of energy purchases and sales. 

![](https://github.com/vladimir-dinolov/Portfolio/blob/main/images/Hydro%20Data.PNG?raw=true)

Packages used for this project: class, rsample, e1071, caret, plyr, dplyr, tidyr, lubridate, zoo, ggplot2, tm, stringr, wordcloud, plyr, factoextra, mclust, and more. 

## Outcomes and Recommendations 
### 1. Reliability of Service
If the hydro utility company provides unreliable services, this could cause customer churn. The company must provide dependable energy to its residential, industrial, and commercial customers. A loss of load events, or a lack of supply for demand, could cost billions of dollars in revenue loss for consumers. This loss will lead consumers to explore the market and seek different utility providers. 

### 2. Affordability of Service
To mitigate a loss in affordability, the company must flexibly plan when, how much, and what type of energy product to stockpile. Ideally, this would be the result of a combination of predictive analytics and proactive problem solving. 

### 3. Renewable Energy Sources
The company must continuously adjust its hydroelectric supply chain to respond to hydro volatility caused by changes in load, flow, outage numbers, net hydro assets, and profit. Moreover, climate change affects Seattle City Light’s net hydro assets and adds a level of unexpected variability. In response to these changes, the company should strategically support its reliability and affordability plans by maintaining a pulse on wind and solar energy markets along with current and forecasted renewable energy market conditions. This will result in the company adjusting its renewable energy supply on a monthly basis by adding stockpiled energy or selling energy reserves to its competitors. 

The Syracuse team had the following recommendations for the hydro utility company: 

• Apply data mining methodologies to predict when to stockpile energy in preparation for changes in flow, load, profit, energy supply and outages. 
• Monitor and adjust renewable energy reserves to supplement hydro supply; sell excess energy based on market demand. 
• Gather national, regional, and local solar and wind production data as well as natural gas market data to include in future predictive modeling. 
