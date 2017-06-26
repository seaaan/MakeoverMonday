library(tidyverse)
library(readxl)
d <- read_excel("data/2016-week-24-women-in-the-workplace.xlsx")
d <- d %>% 
    mutate(Level = factor(Level, levels = unique(Level)), 
        Year = factor(Year))

ggplot(d, aes(x = Year, y = Female)) + 
    geom_line(aes(color = Level, group = Level)) + 
    geom_hline(yintercept = 0.5)

d %>% 
    gather(Gender, Percent, Male, Female) %>% 
    ggplot(aes(x = Year, y = Percent)) + 
    geom_line(aes(color = Level, group = Level)) + 
    geom_hline(yintercept = 0.5) + 
    facet_wrap(~ Gender) +
    ylim(0:1)

d %>% 
    gather(Gender, Percent, Male, Female) %>% 
    ggplot(aes(x = Year, y = Percent)) + 
        geom_line(aes(color = Level, group = interaction(Level, Gender), linetype = Gender)) + 
        geom_hline(yintercept = 0.5) + 
        ylim(0:1)

ggplot(d, aes(x = Year, y = Female)) + 
    geom_col(aes(fill = Level), position = "dodge") + 
    geom_hline(yintercept = 0.5)

ggplot(d, aes(x = Level, y = Female)) + 
    geom_col(aes(fill = Year), position = "dodge") + 
    geom_hline(yintercept = 0.5)

ggplot(d, aes(x = Year, y = Female)) + 
    geom_col() + 
    geom_hline(yintercept = 0.5) + 
    facet_wrap(~ Level)

d %>% 
    gather(Gender, Percent, Male, Female) %>% 
    ggplot(aes(x = Year, y = Percent, fill = Gender)) + 
        geom_col() + 
        geom_hline(yintercept = 0.5) + 
        facet_grid(Level ~ Gender) + ylim(c(0, 1))

d %>% 
    gather(Gender, Percent, Male, Female) %>% 
    ggplot(aes(x = Gender, y = Percent, fill = Year)) + 
    geom_col(position = "dodge") + 
    geom_hline(yintercept = 0.5) + 
    facet_wrap(~ Level) + ylim(c(0, 1))

d %>% 
    mutate(M = Male-0.5, Fe = Female-0.5) %>% 
    tidyr::gather(Gender, Share, M, Fe) %>% 
    mutate(Share = Share * 100) %>% 
    ggplot(aes(x = Level, y = Share, fill = Gender)) + 
        geom_col(aes(group = Year), position = "dodge") + 
    facet_wrap(~Year) + 
    ylab("Difference from equal representation")

d %>% 
    mutate(ManPerWoman = Male / Female) %>% 
    ggplot(aes(x = Year, y = ManPerWoman)) + 
        geom_col() + facet_wrap(~ Level) + 
        ylab("Number of men employed at Microsoft for every woman") + 
        xlab(NULL)

d %>% 
    mutate(ManPerWoman = Male / Female, 
        Level = stringr::str_replace(Level, " / ", " or "), 
        Level = stringr::str_replace_all(Level, " ", "\n"),
        Level = factor(Level, levels = unique(Level))) %>%
    ggplot(aes(x = Level, y = ManPerWoman)) + 
    geom_col(aes(fill = Year), position = "dodge") + 
    ylab("Number of men employed at Microsoft for every woman") + 
    xlab(NULL)

# prettify
d %>% 
    mutate(ManPerWoman = Male / Female, 
        Level = stringr::str_replace(Level, " / ", " or "), 
        Level = stringr::str_replace_all(Level, " ", "\n"),
        Level = factor(Level, levels = unique(Level))) %>%
    ggplot(aes(x = Level, y = ManPerWoman)) + 
        geom_col(aes(fill = Year), position = "dodge") + 
        ylab("Number of men employed per woman") + 
        xlab(NULL) + 
        ggtitle("Gender inequality at Microsoft") + 
        labs(subtitle = paste("Microsoft employs more men than women in every job category",
            "especially at the highest levels, though modest improvements were",
            "made between 2012 and 2015", sep = "\n"), fill = NULL) + 
    theme(legend.position=c(0.07, 0.9), 
        # legend.box.background = element_rect(fill = "transparent"))
        legend.box.just = "top",
        legend.box.background = element_rect(fill = alpha("red", 0.5)))
        