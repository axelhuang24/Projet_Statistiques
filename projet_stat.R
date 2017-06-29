load("./dataProject.RData")
summary(data)
#1.2
boxplot(data$abs~data$Sex)
#1.3
boxplot(data$abs~data$Age)
#1.4
hist(data$abs)


#2.1
lambda = mean(data$abs) ;
plot(data$abs,dpois(data$abs,lambda,FALSE))


#2.3 et 2.4
vecteur_lambda = seq(0,20,0.1);
vecteur_n=1:150;
vraisemblance = seq(0,20,0.1);

poiss <- function(lambda,vecteur_n){
  s=0;
  for (k in vecteur_n) {
    s=s+data$abs[k]*log(lambda,base = exp(1))-log(factorial(data$abs[k]));
  }
  n=length(vecteur_n);
  s=s-n*lambda;
  return(s)
}

i=1;
for (la in vecteur_lambda) {
  
  vraisemblance[i]=poiss(la,vecteur_n);
  i=i+1;
}

plot(vecteur_lambda,vraisemblance)
abline(v=lambda,col="red")

#3.1
borne_g=lambda-1.96/(sqrt(150*(1/lambda))); 
borne_d=lambda+1.96/(sqrt(150*(1/lambda)))


#4.1
nb_G=0;
nb_F=0;
sum_G=0;
sum_F=0;
for(i in vecteur_n)
{
  if(data$Sex[i]=='F')
  {
   nb_F <- nb_F+1;
   sum_F <- sum_F+data$abs[i];
  }else
  {
    nb_G <- nb_G+1;
    sum_G <- sum_G+data$abs[i];
  }

}
m_G=sum_G/nb_G;
m_F=sum_F/nb_F



#5.2
model <- glm(data$abs~data$Age,family = poisson(link = "log"))
summary(model)





