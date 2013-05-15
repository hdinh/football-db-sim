# Football db sim

# Description

The idea was to use the football play by play data provided at http://www.advancednflstats.com/2010/04/play-by-play-data.html to create some sort of football play simulator.
Basically, we take the play by play data, put it in a database (because at the time I was looking for an excuse to play with a ralational database), and query the database for what the "average" play call and play result for a given "football situation" is.

A "football situation" is the current state of the game at that time. So for example, if a team is down by 5 points in the fourth quarter, and they are at the 20 yard line with only 1 minute left to play. More than likely will choose to pass the ball and more than likely the amount of yards they gain will either be 0 yards or enough for a touchdown.

There is no smart AI or anything - we're just querying the database for what happened for most teams in that situation. Also, the defense is completely ignored - only the offense is considered.

But really, this was just an excuse to play around with clojure and sql...

# Usage

* Untar the data contained in data/pbp.tar.gz
* Initialize the sqlite database (this will take a long time) by running:

        lein run -m football.init-data

* Run the game simulator by running:
  
        lein run
