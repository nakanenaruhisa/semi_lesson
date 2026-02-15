###パラメータ
#人口の男女比
male <- 6143 #万人
female <- 6480 #万人

#男女の身長、BMIの値
male.height.mean <- 171.7 #cm
male.height.sd <- 6.6 #cm
male.bmi.mean <- 23.12
male.bmi.sd <- 4.24
female.height.mean <- 158.3 #cm
female.height.sd <- 5.7 #cm
female.bmi.mean <- 20.82
female.bmi.sd <- 3.42

#血液型の割合
A <- 0.39
B <- 0.22
O <- 0.29
AB <- 0.10

#データ数
N <- 1000


###男女のベクトルを作成
mf <- c()
for(i in 1:N){
  tmp <- runif(1)
  if (tmp < (male/(male+female))) mf <- c(mf, "male")
  else                            mf <- c(mf, "female")
}

###血液型のベクトルを作成
bt <- c()
for(i in 1:N){
  tmp <- runif(1)
  if (tmp < A)                           bt <- c(bt, "A")
  else if (A <= tmp & tmp < (A+B))       bt <- c(bt, "B")
  else if ((A+B) <= tmp & tmp < (A+B+O)) bt <- c(bt, "O")
  else                                   bt <- c(bt, "AB")
}

###身長、体重のベクトルを作成
h <- c()
w <- c()
for (i in 1:N) {
  if (mf[i] == "male") {
    height <- rnorm(n=1,mean=male.height.mean,sd=male.height.sd)
    bmi <- rnorm(n=1,mean=male.bmi.mean,sd=male.bmi.sd)
  } else {
    height <- rnorm(n=1,mean=female.height.mean,sd=female.height.sd)
    bmi <- rnorm(n=1,mean=female.bmi.mean,sd=female.bmi.sd)
  }
  weight <- (height * 0.01)^2 * bmi
  h <- c(h, height)
  w <- c(w, weight)
}

###data.frame化
df <- data.frame(sex=mf,height=h,weight=w,blood=bt)

head(df)
write_csv(df, "height/testdataset.csv")
