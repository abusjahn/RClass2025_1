pacman::p_load(wrappedtools, tidyverse)
pop <- tibble(size=rnorm(10^5,170,10))
ggplot(pop,aes(size))+
  geom_histogram(fill='lightgrey',color='black',
                 binwidth = 10)+
  scale_y_continuous(breaks=seq(0,10^5,2000))+
  scale_x_continuous(breaks=seq(0,300,10))
ggplot(pop,aes(size))+
  geom_histogram(fill='lightgrey',color='black',
                 binwidth = 5)+
  scale_y_continuous(breaks=seq(0,10^5,2000))+
  scale_x_continuous(breaks=seq(0,300,10))
ggplot(pop,aes(size))+
  geom_histogram(color='black',
                 binwidth = 1,
                 aes(fill=size>=190.5|size<=150.5))+
  scale_y_continuous(breaks=seq(0,10^5,200))+
  scale_x_continuous(breaks=seq(0,300,10))
ggplot(pop,aes(size))+
  scale_y_continuous(breaks=seq(0,10^5,200))+
  scale_x_continuous(breaks=seq(0,300,10))+
  geom_histogram(color='black',bins = 200,
                 aes(fill=size>=190))
table(pop$size>=190)
ggplot(pop,aes(size))+
  scale_x_continuous(breaks=seq(0,300,5))+
  geom_density()
pop |>
  slice_sample(prop = .001) |>
  ggplot(aes(size))+
  scale_x_continuous(breaks=seq(0,300,5))+
  geom_density()

n_sim=10^4
chi_empiric <- tibble(chi2=rep(NA_real_, n_sim))

for(rep_i in seq_len(n_sim)){
  M1 <- sample(c(1:3),size = 100,replace = T)
  M2 <- sample(c('A','B'),size = 100,replace = T)
  chi_empiric$chi2[rep_i]  <-  chisq.test(M1,M2)$statistic
}
table(M1,M2)
prop.table(table(M1,M2),margin = 1)*100
chisq.test(M1,M2)

chi2_threshold5 <- qchisq(p = .95,df = 2)
ggplot(chi_empiric,aes(chi2))+
  geom_histogram(fill='lightgrey',color='black',bins=50)
ggplot(chi_empiric,aes(chi2))+
  geom_histogram(aes(fill=chi2>=chi2_threshold5),
                 color='black',bins=50)
tempplot <- ggplot(chi_empiric,aes(chi2))+
  geom_density()+
  scale_x_continuous(breaks=seq(0,300,1))+
  coord_cartesian(xlim = c(0,15))
tempplot
d <- ggplot_build(tempplot)$data[[1]]
tempplot+
  geom_area(data = d %>% filter(x>=chi2_threshold5,x<=15),
            aes(x,y),
            fill='orangered',alpha=.5)+
  annotate(geom = 'label',x=chi2_threshold5,y=.1,
           label='AUC=0.05',
           hjust=0)



t_empiric <- tibble(t=rep(NA_real_,10^4),
                    delta=NA_real_)
for(rep_i in 1:10^4){
  sample1 <- rnorm(20,mean = 170,sd = 10)
  sample2 <- rnorm(20,mean = 170,sd = 10)
  t_empiric$delta[rep_i] <-
    mean(sample1)-mean(sample2)
  t_empiric$t[rep_i] <-  t.test(sample1,sample2,
                                var.equal = T)$statistic
}
ggplot(t_empiric,aes(t))+
  geom_histogram(color='black',bins=100,
                 aes(fill=t>=1.4|t<=-1.4))
ggplot(t_empiric,aes(t))+
  geom_density()
t_empiric %>%
  pivot_longer(everything()) %>%
  ggplot(aes(value))+
  geom_density()+
  facet_grid(rows = vars(name),
             scales = 'free')



# normalgroesse <- tibble(size=rnorm(10^5,170,10))
tempplot <- ggplot(pop,aes(size))+
  geom_density(alpha=.5)+
  scale_x_continuous(breaks=seq(0,300,10))+
  # geom_vline(xintercept = c(10,26))+
  coord_cartesian(xlim = c(130,210))
d <- ggplot_build(tempplot)$data[[1]]
tempplot+
  geom_area(data = d %>% filter(x>=160,x<=180),aes(x,y),
            fill='lightgreen',alpha=.5)+
  geom_area(data = d %>% filter(x<160,x>=150),aes(x,y),
            fill='yellow',alpha=.5)+
  geom_area(data = d %>% filter(x>180,x<=190),aes(x,y),
            fill='yellow',alpha=.5)+
  geom_area(data = d %>% filter(x<150),aes(x,y),
          fill='orangered',alpha=.5)+
  geom_area(data = d %>% filter(x>190),aes(x,y),
            fill='orangered',alpha=.5)+
  geom_vline(xintercept = c(160,180),color='grey')+
  annotate(geom = 'label',x=180,y=.035,
           label='Mean + SD\n170+10',
           hjust=0)+
  annotate(geom = 'label',x=160,y=.035,
           label='Mean - SD\n170-10',
           hjust=1)
# "true" thrshold for 5%:
qnorm(p = .975,mean = 0,sd = 1) # not 2 sigma, but 1.96
