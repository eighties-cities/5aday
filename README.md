# 5aday

A multi agent model to simulate the effects of social and temporal segregation on diet disparities.

## Install

You first neet to install h24 (sbt publishLocal)
Then:
sbt osgiBundle

## Run a single simulation

```
sbt -J-Xmx2G "runMain eighties.fiveaday.run.SimulationApp -d ./data/initialisation_distribution_per_cat_2002_2008.csv -p ../h24/results_IDF/population.bin -m ../h24/results_IDF/moves.bin"
```

## Run all scenarios

```
sbt -J-Xmx4G "runMain eighties.fiveaday.run.SimulationWithMapApp -d ./data/initialisation_distribution_per_cat_2002_2008.csv -p ../h24/results_IDF/population.bin -r ../h24/results_IDF/randomPopulation.bin  -m ../h24/results_IDF/moves.bin -o /tmp/map" 
```
