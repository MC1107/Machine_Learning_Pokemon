# Machine Learning Pokemon

## Final project for Data Mining and Machine Learning course (Fall 2017)
## Data Description
The original dataset contains 721 Pokémon observations with 12 variables, including Name,
Type 1, Type 2, Total, HP, Attack, Defense, Sp. Atk, Sp. Def, Speed, Generation and
Legendary.
## Project Goals
## This project aims to apply various statistical methods to construst machine learning models to predict:
● Which variable(s) can define the type of a Pokémon, if any? Also interpret the final
model.
● Whether a Pokémon is legendary or not.

## Data Cleansing
We combine Type 1 and Type 2 into one column named TypeCombine, by
treating those Pokémon who possess two types as two independent observations. For the
ease of the analysis, TypeCombine and Legendary are renamed as type and leg, and then
are recoded from categorical variables into discrete numbers as type.num and leg.num.
Following this, the recoded variables are attached to the original dataset. The updated dataset
now has 1214 observations with 14 variables. Variable Total is omitted from the analysis to
prevent the problem of multicollinearity because it is
