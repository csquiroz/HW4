#### Homework 4 ####
### Cody Quiroz - Zoo 800 ###
## 25-Sep-25 ##


## Objective 1 ##
#install.packages('palmerpenguins')
library('palmerpenguins')
View(penguins) #view the dataset

make_binary = function(x, breakpoint, labels = c("low", "high")) { #make function using generic variables
  binary = ifelse(x <= breakpoint, labels[1], labels[2]) #if <= breakpoint, will assign "low". if >bp, will assign "high".
  print(binary) #prints outcomes
}

breakpoint <- median(penguins$body_mass_g, na.rm = TRUE) #using the median weight as the breakpoint. na.rm=TRUE ignores NAs

penguins$sizeclass = make_binary( #making new column called sizeclass.
  x = penguins$body_mass_g, #assigning x as mass
  breakpoint = breakpoint, #setting breakpoint
  labels = c("Small","Large") #setting labels
)


## Objective 2 ##
make_many = function(x, breakpoints, labels) { #sets variables we'll use in the function
  groups = cut(x, breaks = breakpoints, labels = labels, include.lowest = TRUE) #cut to make categories and assign function variables to equation variables
  print(groups) #shows us what we see
}

#calculating even split to assign breakpoints for 3 categories
range(penguins$body_mass_g, na.rm=TRUE) #determine high and low
diff <- 6300-2700 #find difference
third_diff <- diff/3 #evenly split into 3
breakpoints <- c(2700,3900,5100,6300) #set breakpoints by thirds

penguins$sizeclass = make_many( #re-write size class column
  x = penguins$body_mass_g, #assigning x as mass
  breakpoints = breakpoints, #setting breakpoints as defined above
  labels = c("Small","Medium","Large") #sets label names for 3 categories
)


## Objective 3 ##
make_size_class_sp <- function(x, species) {
  #finding breakpoints by each species
  adelie_bp <- quantile(penguins$body_mass_g[penguins$species == "Adelie"], na.rm = TRUE, probs = c(1/3,2/3)) #finding adelie tertiles
  gentoo_bp <- quantile(penguins$body_mass_g[penguins$species == "Gentoo"], na.rm = TRUE, probs = c(1/3,2/3)) #finding gentoo tertiles
  chinstrap_bp <- quantile(penguins$body_mass_g[penguins$species == "Chinstrap"], na.rm = TRUE, probs = c(1/3,2/3)) #finding chinstrap tertiles
  
  size_class_sp <- rep(NA, length(x)) #empty vector for output
  
  #loop through each penguin
  for (i in seq_along(x)) {
    if (is.na(x[i])) { #for NAs
      size_class_sp[i] <- NA #if NA, makes size class NA
    } else if (species[i] == "Adelie") { #for Adelies
      size_class_sp[i] <- ifelse(x[i] <= adelie_bp[1], "Small", #if Adelie body mass (x defined as such after) is lower than first breakpoint, "small"
                              ifelse(x[i] <= adelie_bp[2], "Medium", "Large")) #of whats still undefined after above, if Adelie body mass is less than bp 2, "medium". otherwise, it is "large" if left over.
    } else if (species[i] == "Gentoo") { #for Gentoos
      size_class_sp[i] <- ifelse(x[i] <= gentoo_bp[1], "Small", #if Gentoo mass is lower than first bp,"small"
                              ifelse(x[i] <= gentoo_bp[2], "Medium", "Large")) #but of whats still undefined, if Gentoo mass is lower than second bp, "medium". Otherwise, "large" if left
    } else if (species[i] == "Chinstrap") { #for Chinstraps
      size_class_sp[i] <- ifelse(x[i] <= chinstrap_bp[1], "Small", #if Chinstrap mass is lower than first bp, "small"
                              ifelse(x[i] <= chinstrap_bp[2], "Medium", "Large")) #of what's still undefined, if Chinstrap mass is less than bp 2, "medium". Otherwise, "large" if left
    }
  }
  return(size_class_sp)
}

##assigning variables for long function above
penguins$size_class_sp <- make_size_class_sp( #using above function and making new column
  x = penguins$body_mass_g, #x as body mass
  species = penguins$species #species as species
)

penguins$sizeclass <- NULL #removing previous size class column that was not separated by species
View(penguins) #admiring the fruits of my labor

table(penguins$species, penguins$size_class_sp, useNA = "ifany") #making a table of how many in each size class per species for funsies


## Objective 4 ##

make_size_class_sp <- function(x, species) {
  #finding breakpoints by species
  adelie_bp <- quantile(penguins$body_mass_g[penguins$species == "Adelie"], na.rm = TRUE, probs = c(1/3,2/3)) #finding adelie tertiles
  gentoo_bp <- quantile(penguins$body_mass_g[penguins$species == "Gentoo"], na.rm = TRUE, probs = c(1/3,2/3)) #finding gentoo tertiles
  chinstrap_bp <- quantile(penguins$body_mass_g[penguins$species == "Chinstrap"], na.rm = TRUE, probs = c(1/3,2/3)) #finding chinstrap tertiles
  
  size_class_sp <- rep(NA, length(x)) #empty vector for output
  
  for (i in seq_along(x)) { #loop thru each penguin
    if (is.na(x[i])) { #for NAs
      size_class_sp[i] <- NA #if NA, makes size class NA
    } else if (species[i] == "Adelie") { #for Adelies
      size_class_sp[i] <- ifelse(x[i] <= adelie_bp[1], "Small", #if Adelie body mass (x defined as such after) is lower than first breakpoint, "small"
        ifelse(x[i] <= adelie_bp[2], "Medium", "Large")) #of whats still undefined after above, if Adelie body mass is less than bp 2, "medium". otherwise, it is "large" if left over.
    } else if (species[i] == "Gentoo") { #for Gentoos
      size_class_sp[i] <- ifelse(x[i] <= gentoo_bp[1], "Small", #if Gentoo mass is lower than first bp,"small"
        ifelse(x[i] <= gentoo_bp[2], "Medium", "Large")) #but of whats still undefined, if Gentoo mass is lower than second bp, "medium". Otherwise, "large" if left
    } else if (species[i] == "Chinstrap") { #for Chinstraps
      size_class_sp[i] <- ifelse(x[i] <= chinstrap_bp[1], "Small", #if Chinstrap mass is lower than first bp, "small"
        ifelse(x[i] <= chinstrap_bp[2], "Medium", "Large")) #of what's still undefined, if Chinstrap mass is less than bp 2, "medium". Otherwise, "large" if left
    }
  }
 
library(ggplot2) #load ggplot
library(dplyr) #load dplyer for pipe for filtering out NAs
library(forcats) #load forcats to reverse the order of the x-axis

penguins %>%
  filter(!is.na(size_class_sp)) %>% #filter out NAs so there isnt an empty space on plot
  
ggplot(aes(x = fct_rev(size_class_sp), y = body_mass_g, fill = species)) + #x axis is size class (reversed to go from S>M>L), y is body mass. color based on species
  geom_boxplot() + #make it a box plot
  scale_fill_brewer("Species", palette = "Set3") +
  labs( #make nice labels for axes
    x = "Size Class",
    y = "Body Mass (g)") + 
  theme(axis.title.y = element_text(vjust = 0.5, angle=0)) #turn y label sideways for readability
  theme_minimal() #make it nice and clean

return(size_class_sp)
}


## Objective 5 ##

make_size_class_any <- function(x, species) {#making function
  size_class_any <- rep(NA, length(x))
  for (sp in unique(species)) {
    peng <- which(species == sp) #goes thru each penguin (indexing?). if row contains the given sp uses only those rows
    bp <- quantile(x[peng], probs = c(1/3, 2/3), na.rm = TRUE) #break point for each peng species, splitting into thirds
    size_class_any[peng] <- as.character(cut( #find size class for each penguin species. as.character() enusres S/M/L. otherwise, it'll spit out 1/2/3 for some reason
      x[peng], #weight
      breaks = c(-Inf, bp[1], bp[2], Inf), #defining breaks. neg. infinity as a minimum, first bp, second bp, and Inf to allow for any size
      labels = c("Small", "Medium", "Large")))
      }
  return(size_class_any)
  }

#test using our penguin data
penguins$size_class_any <- make_size_class_any( #using above function and making new column
  x = penguins$body_mass_g, #x as body mass
  species = penguins$species #species as species
)
