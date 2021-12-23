# Analysis of `lichess` games
Two part project for analysing games played in [lichess](lichess.org). 

Part 1) Data engineering: I created a script to parse and condense the chess games available in the lichess database: https://database.lichess.org/
The games that are included in the analysis are only those that have an evaluation available (around 6% of the games according to the lichess documentation).

Part 2) Data analysis: Naturally I could not help myself and I went and analysed the wonderful dataset that was created in step 1 to see whether I could get insights about the playing habbits of chess masters broken down by theri rating.
https://www.kaggle.com/noobiedatascientist/analysis-of-lichess-games?scriptVersionId=55683619

Some visuals from the notebook:

Popularity of openings accross rating categories:  
![pop_change-1](https://user-images.githubusercontent.com/56187121/147279240-93b948a1-93d5-446f-b875-431c0882b809.png)

Number of times the balance of a game changes by rating and time control:  
![game_flips-1](https://user-images.githubusercontent.com/56187121/147279315-fa1e9931-8557-4b6d-9be4-63f5bb9fd878.png)

Errors per move by time availability and rating:  
![ts_analysis-1](https://user-images.githubusercontent.com/56187121/147279386-f16467e4-5f67-4808-95a5-537d464bdaba.png)
