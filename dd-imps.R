library(tidyverse)
library(gghighlight)

scenes <- expand_grid(m = c(0.1,0.3,0.9),
                      u = seq(0, 1, by = 0.05),
                      dd = c("global","local"),
                      mpa = 0.3)

k <- 1

r <- 0.4

mpa <- 0.25

# m <- 0.3
# 
# move <- m * (1 - mpa) * (25  - mpa / (1 - mpa) * 75)
# 
# move
# 
-m * 50 * (50/50 - 1 / 50) + m * 1 * (1 / 50 - 50 / 50)
# 
# mu * (last_inside_b - (p_mpa / (1 - p_mpa)) * last_outside_b);



popfoo <- function(m,u,r, mpa, k, years = 100, dd = "global") {
  
  b <- matrix(NA, nrow = years, ncol = 2)
  
  b[1, ] <- c(k * mpa, k * (1 - mpa))
  
  mu <- m * (1 - mpa)
  
  h <- rep(NA, years)
  
  for (i in 2:years) {
  
    
    if (dd == "global"){
    # inside MPA
     b[i, 1] <-
      b[i - 1, 1] + mpa * r * (sum(b[i - 1, ])) * (1 - sum(b[i - 1, ]) / k) - mu * (b[i - 1, 1] - (mpa / (1 - mpa)) * b[i - 1, 2])
    
     
     
     # outside MPA
     
     b[i, 2] <-
       (1 - u) * b[i - 1, 2] + (1 - mpa) * r * (sum(b[i - 1, ])) * (1 - sum(b[i - 1, ]) / k) + mu * (b[i - 1, 1] - (mpa / (1 - mpa)) * b[i - 1, 2])
     
     
    } else {
      
      
      b[i, 1] <-
        b[i - 1, 1] +  r * b[i - 1,1] * (1 - b[i - 1, 1] / max(1e-3,(k * mpa))) - mu * (b[i - 1, 1] - (mpa / (1 - mpa)) * b[i - 1, 2])


      # outside MPA

      b[i, 2] <-
        (1 - u) * b[i - 1, 2] + r * b[i - 1, 2] * (1 - b[i - 1 , 2] / max(1e-3,(k * (1 - mpa)))) + mu * (b[i - 1, 1] - (mpa / (1 - mpa)) * b[i - 1, 2])


      # b[i, 1] <-
      #   b[i - 1, 1] +  r * b[i - 1,1] * (1 - b[i - 1, 1] / max(1e-3,(k * mpa))) - m *  b[i - 1,1] * ( b[i - 1,1] / (k * mpa) - b[i - 1,2] / (k * (1 - mpa))) + m *  b[i - 1,2] * ( b[i - 1,2] / (k * (1-mpa)) - b[i - 1,1] / (k * mpa))
      # 
      # 
      # # outside MPA
      # 
      # b[i, 2] <-
      #   (1 - u) * b[i - 1, 2] + r * b[i - 1, 2] * (1 - b[i - 1 , 2] / max(1e-3,(k * (1 - mpa)))) + m *  b[i - 1,1] * ( b[i - 1,1] / (k * mpa) - b[i - 1,2] / (k * (1 - mpa))) - m *  b[i - 1,2] * ( b[i - 1,2] / (k * (1-mpa)) - b[i - 1,1] / (k * (mpa)))

      
      
      
    }
    
    h[i-1] <- u *  b[i - 1, 2] 
    
  }
  
  h[i] <- u * b[i,2]
  
  
  check <- ((m * k * (1 - mpa)) / (u * mpa + m)) * (1 - (u * (1 - mpa) * m)/((u * mpa + m) * r))
  
  check / b[years,2]
  
  out <- b %>% 
    data.frame() %>% 
    mutate(year = 1:years)
  
  colnames(out) <- c("inside",'outside',"year")
  
  out$h <- h
  
  return(out)
  
  # out %>%
  #   ggplot(aes(1:nrow(.), inside)) + 
  #   geom_line()
  
}

scenes <- scenes %>% 
  mutate(huh = pmap(list(m = m,u = u, dd = dd, mpa = mpa),popfoo, k = k, years = 100, r = r))

scenes <- scenes %>% 
  unnest(cols = huh)

scenes %>% 
  filter(year == max(year)) %>% 
  ggplot(aes(u,inside / k, color = factor(m))) + 
  geom_line() + 
  labs(x = "Harvest Fraction",
       y = "Biomass Inside Reserve as a Fraction of K",
       title = paste0(scales::percent(mpa), ' in MPA')) + 
  scale_color_discrete(name = "Movement") + 
  facet_grid(mpa~dd)

scenes %>% 
  filter(year == max(year)) %>% 
  ggplot(aes(u,outside / k, color = factor(m))) + 
  geom_line() + 
  labs(x = "Harvest Fraction",
       y = "Biomass Outside Reserve as a Fraction of K",
       title = paste0(scales::percent(mpa), ' in MPA')) + 
  scale_color_discrete(name = "Movement") + 
  facet_grid(mpa~dd)



msy <- (r * k) / 4
scenes %>% 
  filter(year == max(year)) %>% 
  ggplot(aes(u,h / msy, color = factor(m))) + 
  geom_line() + 
  labs(x = "Harvest Fraction",
       y = "Catch / MSY",
       title = paste0(scales::percent(mpa), ' in MPA')) + 
  scale_color_discrete(name = "Movement") + 
  facet_grid(mpa~dd)

scenes %>% 
  filter(year == max(year)) %>% 
  select(u,h,m,mpa,dd) %>% 
  pivot_wider(names_from = dd, values_from = h) %>% 
  mutate(dd_effect = global - local) %>% 
  ggplot(aes(u,dd_effect / msy, color = factor(m))) + 
  geom_line() + 
  labs(x = "Harvest Fraction",
       y = "Effect of Global Density Dependence as Fraction of MSY",
       title = paste0(scales::percent(mpa), ' in MPA')) + 
  scale_color_discrete(name = "Movement") 


scenes %>% 
  filter(m == 0.9) %>% 
  View()
