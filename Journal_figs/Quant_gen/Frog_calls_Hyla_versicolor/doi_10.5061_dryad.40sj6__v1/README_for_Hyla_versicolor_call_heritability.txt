Hyla_versicolor_call_heritability.csv

Data from: Welch AM, Smith MJ, Gerhardt HC. 2014. A multivariate analysis of
           genetic variation in the advertisement call of the gray treefrog,
           Hyla versicolor.  Evolution.
 

**Overview**

Advertisement call properties recorded from two generations of gray treefrogs (Hyla
versicolor) including field-collected sires and their laboratory-reared sons.  Calls
from each individual were recorded in a simulated social environment consisting of
an array of speakers broadcasting synthetic calls in a semi-anechoic chamber.


**Description of data**

id – a unique number for each frog in the pedigree; hereafter, "identity"
           1-11 = parental generation, dams
           12-59 = parental generation, sires
           60-132 = parental generation, unselected males
           133-476 = offspring generation, sons

sire – identity of the individual’s sire; individuals in the parental generation
           were arbitrarily assigned 600 for sire identity to indicate that the
           sire is not within the pedigree

dam – identity of the individual’s dam; individuals in the parental generation
           were arbitrarily assigned 600 for dam identity to indicate that the 
           dam is not within the pedigree

year – 1 = parental generation, first year
       2 = parental generation, second year
       3 = offspring generation, cohort resulting from first-year parents
       4 = offspring generation, cohort resulting from second-year parents

diet - 0 = no diet treatment (parental generation)
       1 = low food diet
       2 = high food diet

obs[ ,1] - pulse number, standardized*
obs[ ,2] - call period, standardized*
obs[ ,3] - dominant frequency, standardized*

           * data were log-transformed (except for dominant frequency) and then
             standardized to mean of zero and standard deviation of one within
             each cohort for each generation

PN - pulse number, raw
CP - call period, raw
DF - dominant frequency, raw
