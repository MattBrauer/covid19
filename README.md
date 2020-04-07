# covid19

R scripts to plot COVID19 data from the JHU CSSE and NY TIMES sites.

To use, clone this repo then add the data repos as submodules:

```
git clone git@github.com:MattBrauer/covid19.git
cd covid19
```
JHU CSSE:
```
git submodule add git@github.com:CSSEGISandData/COVID-19.git
cd COVID-19
git submodule init
git submodule update --remote
```
NY Times:
```
git submodule add git@github.com:nytimes/covid-19-data.git
cd covid-19-data
git submodule init
git submodule update --remote
```

Data updates:
`git submodule update --remote` 

# line from Rstudio
