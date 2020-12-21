library("survival")
library("survminer")
library("gdata")
df <- read.xls('/Users/yangyunchen/Google Drive/Pre post info/r_analysis.xlsx', sheet=1, na.strings=c("NA", "#DIV/0!"))

df$gender_mat <- as.numeric(sub("F","0",sub("M","1",df$Gender)))
df$location_mat <- as.numeric(sub("Right","0",sub("Left","1",df$LATERAL)))
df$smoker_mat <- as.numeric(sub("Nonsmoker", "0", sub("Former","0", sub("Current","1",df$Smoker))))
df$grade_mat <- as.numeric(sub("Well differentiated", "1", sub("Moderately differentiated","1", sub("Poorly differentiated","0",df$Grade))))
df$status_mat <- as.numeric(sub("lost follow up", "0", sub("Alive","0", sub("Dead","1",df$Post.status))))
df$reoccur_status_mat <- as.numeric(sub("No","0",sub("Yes","1",df$Reoccur.status)))
df$reoccur_mat <- df$reoccur_status_mat
df$reoccur_mat[df$status_mat==1|df$reoccur_status_mat==1]=1

fit <- survfit(Surv(df$survival, df$status_mat) ~ gender_mat, data=df)
p <- ggsurvplot(fit, xlab = "Follow up time (Months)",
                conf.int = TRUE, 
                pval=paste0('Log-rank\np = ', round(surv_pvalue(fit)$pval,4)),
                ylim = c(0.25, 1),
                pval.coord=c(10,0.5),
                legend.labs=c("Female","Male"),
                font.tickslab = c(16),
                legend.title="Gender",
                ggtheme = theme_classic2(base_size=16, base_family = "Arial"),
                font.family = "Arial"
)
ggpar(p, 
      font.main = c(16, "bold", "black"),
      font.submain = c(16, "bold", "black"),
      font.subtitle = c(16, "bold", "black"),
      font.x = c(16, "bold", "black"),
      font.y = c(16, "bold", "black"),
      font.caption = c(16, "bold", "black"), 
      font.legend = c(16, "bold", "black"), 
      font.tickslab = c(16, "bold", "black"),
      legend = 'top')

fit <- survfit(Surv(df$survival, df$status_mat) ~ location_mat, data=df)
p <- ggsurvplot(fit, xlab = "Follow up time (Months)",
                conf.int = TRUE, 
                pval=paste0('Log-rank\np = ', round(surv_pvalue(fit)$pval,4)),
                ylim = c(0.25, 1),
                pval.coord=c(10,0.5),
                legend.labs=c("Right","left"),
                font.tickslab = c(16),
                legend.title="Lateral",
                ggtheme = theme_classic2(base_size=16, base_family = "Arial"),
                font.family = "Arial"
)
ggpar(p, 
      font.main = c(16, "bold", "black"),
      font.submain = c(16, "bold", "black"),
      font.subtitle = c(16, "bold", "black"),
      font.x = c(16, "bold", "black"),
      font.y = c(16, "bold", "black"),
      font.caption = c(16, "bold", "black"), 
      font.legend = c(16, "bold", "black"), 
      font.tickslab = c(16, "bold", "black"),
      legend = 'top')

fit <- survfit(Surv(df$survival, df$status_mat) ~ smoker_mat, data=df)
p <- ggsurvplot(fit, xlab = "Follow up time (Months)",
                conf.int = TRUE, 
                pval=paste0('Log-rank\np = ', round(surv_pvalue(fit)$pval,4)),
                ylim = c(0.25, 1),
                pval.coord=c(10,0.5),
                legend.labs=c("Former smokers\n/Non-smokers","Current\nsmokers"),
                font.tickslab = c(16),
                legend.title="Smoking",
                ggtheme = theme_classic2(base_size=16, base_family = "Arial"),
                font.family = "Arial"
)
ggpar(p, 
      font.main = c(16, "bold", "black"),
      font.submain = c(16, "bold", "black"),
      font.subtitle = c(16, "bold", "black"),
      font.x = c(16, "bold", "black"),
      font.y = c(16, "bold", "black"),
      font.caption = c(16, "bold", "black"), 
      font.legend = c(16, "bold", "black"), 
      font.tickslab = c(16, "bold", "black"),
      legend = 'top')

fit <- survfit(Surv(df$survival, df$status_mat) ~ grade_mat, data=df)
p <- ggsurvplot(fit, xlab = "Follow up time (Months)",
                conf.int = TRUE, 
                pval=paste0('Log-rank\np = ', round(surv_pvalue(fit)$pval,4)),
                ylim = c(0.25, 1),
                pval.coord=c(10,0.5),
                legend.labs=c("Poorly\ndifferentiated","Moderately/Well\ndifferentiated"),
                font.tickslab = c(16),
                legend.title="Grade",
                ggtheme = theme_classic2(base_size=16, base_family = "Arial"),
                font.family = "Arial"
)
ggpar(p, 
      font.main = c(16, "bold", "black"),
      font.submain = c(16, "bold", "black"),
      font.subtitle = c(16, "bold", "black"),
      font.x = c(16, "bold", "black"),
      font.y = c(16, "bold", "black"),
      font.caption = c(16, "bold", "black"), 
      font.legend = c(16, "bold", "black"), 
      font.tickslab = c(16, "bold", "black"),
      legend = 'top')

fit <- survfit(Surv(df$recurrence, df$reoccur_mat) ~ gender_mat, data=df)
p <- ggsurvplot(fit, xlab = "Follow up time (Months)",
                ylab = "Reoccur probability",
                conf.int = TRUE, 
                pval=paste0('Log-rank\np = ', round(surv_pvalue(fit)$pval,4)),
                ylim = c(0.25, 1),
                pval.coord=c(10,0.5),
                legend.labs=c("Female","Male"),
                font.tickslab = c(16),
                legend.title="Gender",
                ggtheme = theme_classic2(base_size=16, base_family = "Arial"),
                font.family = "Arial"
)
ggpar(p, 
      font.main = c(16, "bold", "black"),
      font.submain = c(16, "bold", "black"),
      font.subtitle = c(16, "bold", "black"),
      font.x = c(16, "bold", "black"),
      font.y = c(16, "bold", "black"),
      font.caption = c(16, "bold", "black"), 
      font.legend = c(16, "bold", "black"), 
      font.tickslab = c(16, "bold", "black"),
      legend = 'top')

fit <- survfit(Surv(df$recurrence, df$reoccur_mat) ~ location_mat, data=df)
p <- ggsurvplot(fit, xlab = "Follow up time (Months)",
                ylab = "Reoccur probability",
                conf.int = TRUE, 
                pval=paste0('Log-rank\np = ', round(surv_pvalue(fit)$pval,4)),
                ylim = c(0.25, 1),
                pval.coord=c(10,0.5),
                legend.labs=c("Right","left"),
                font.tickslab = c(16),
                legend.title="Lateral",
                ggtheme = theme_classic2(base_size=16, base_family = "Arial"),
                font.family = "Arial"
)
ggpar(p, 
      font.main = c(16, "bold", "black"),
      font.submain = c(16, "bold", "black"),
      font.subtitle = c(16, "bold", "black"),
      font.x = c(16, "bold", "black"),
      font.y = c(16, "bold", "black"),
      font.caption = c(16, "bold", "black"), 
      font.legend = c(16, "bold", "black"), 
      font.tickslab = c(16, "bold", "black"),
      legend = 'top')

fit <- survfit(Surv(df$recurrence, df$reoccur_mat) ~ smoker_mat, data=df)
p <- ggsurvplot(fit, xlab = "Follow up time (Months)",
                ylab = "Reoccur probability",
                conf.int = TRUE, 
                pval=paste0('Log-rank\np = ', round(surv_pvalue(fit)$pval,4)),
                ylim = c(0.25, 1),
                pval.coord=c(10,0.5),
                legend.labs=c("Former smokers\n/Non-smokers","Current\nsmokers"),
                font.tickslab = c(16),
                legend.title="Smoking",
                ggtheme = theme_classic2(base_size=16, base_family = "Arial"),
                font.family = "Arial"
)
ggpar(p, 
      font.main = c(16, "bold", "black"),
      font.submain = c(16, "bold", "black"),
      font.subtitle = c(16, "bold", "black"),
      font.x = c(16, "bold", "black"),
      font.y = c(16, "bold", "black"),
      font.caption = c(16, "bold", "black"), 
      font.legend = c(16, "bold", "black"), 
      font.tickslab = c(16, "bold", "black"),
      legend = 'top')

fit <- survfit(Surv(df$recurrence, df$reoccur_mat) ~ grade_mat, data=df)
p <- ggsurvplot(fit, xlab = "Follow up time (Months)",
                ylab = "Reoccur probability",
                conf.int = TRUE, 
                pval=paste0('Log-rank\np = ', round(surv_pvalue(fit)$pval,4)),
                ylim = c(0.25, 1),
                pval.coord=c(10,0.5),
                legend.labs=c("Poorly\ndifferentiated","Moderately/Well\ndifferentiated"),
                font.tickslab = c(16),
                legend.title="Grade",
                ggtheme = theme_classic2(base_size=16, base_family = "Arial"),
                font.family = "Arial"
)
ggpar(p, 
      font.main = c(16, "bold", "black"),
      font.submain = c(16, "bold", "black"),
      font.subtitle = c(16, "bold", "black"),
      font.x = c(16, "bold", "black"),
      font.y = c(16, "bold", "black"),
      font.caption = c(16, "bold", "black"), 
      font.legend = c(16, "bold", "black"), 
      font.tickslab = c(16, "bold", "black"),
      legend = 'top')

# df$bio21.pre.z <- scale(df$Biosensor.miR.21.pre)
# df$bio486.pre.z <- scale(df$Biosensor.miR.486.pre)
# df$bio24.pre.z <- scale(df$Biosensor.miR.24.pre)
# df$bio30d.pre.z <- scale(df$Biosensor.miR.30d.pre)
# df$bio21.post6.z <- scale(df$Biosensor.miR.21.post.6.months)
# df$bio486.post6.z <- scale(df$Biosensor.miR.486.post.6.months)
# df$bio24.post6.z <- scale(df$Biosensor.miR.24.post.6.months)
# df$bio30d.post6.z <- scale(df$Biosensor.miR.30d.post.6.months)
# 
# df$bio21.fc.z0.6 <- scale(log(df$Biosensor.miR.21.post.6.months/df$Biosensor.miR.21.pre,2))
# df$bio486.fc.z0.6 <- scale(log(df$Biosensor.miR.486.post.6.months/df$Biosensor.miR.486.pre,2))
# df$bio24.fc.z0.6 <- scale(log(df$Biosensor.miR.24.post.6.months/df$Biosensor.miR.24.pre,2))
# df$bio30d.fc.z0.6 <- scale(log(df$Biosensor.miR.30d.post.6.months/df$Biosensor.miR.30d.pre,2))
# df$bio21.diff.z0.6 <- scale(df$Biosensor.miR.21.post.6.months - df$Biosensor.miR.21.pre)
# df$bio486.diff.z0.6 <- scale(df$Biosensor.miR.486.post.6.months - df$Biosensor.miR.486.pre)
# df$bio24.diff.z0.6 <- scale(df$Biosensor.miR.24.post.6.months - df$Biosensor.miR.24.pre)
# df$bio30d.diff.z0.6 <- scale(df$Biosensor.miR.30d.post.6.months - df$Biosensor.miR.30d.pre)

# df$bio21.post12.z <- scale(df$Biosensor.miR.21.post.12.months)
# df$bio486.post12.z <- scale(df$Biosensor.miR.486.post.12.months)
# df$bio24.post12.z <- scale(df$Biosensor.miR.24.post.12.months)
# df$bio30d.post12.z <- scale(df$Biosensor.miR.30d.post.12.months)
# df$bio21.diff.z0.12 <- scale(df$Biosensor.miR.21.post.12.months - df$Biosensor.miR.21.pre)
# df$bio486.diff.z0.12 <- scale(df$Biosensor.miR.486.post.12.months - df$Biosensor.miR.486.pre)
# df$bio24.diff.z0.12 <- scale(df$Biosensor.miR.24.post.12.months - df$Biosensor.miR.24.pre)
# df$bio30d.diff.z0.12 <- scale(df$Biosensor.miR.30d.post.12.months - df$Biosensor.miR.30d.pre)
# df$bio21.diff.z6.12 <- scale(df$Biosensor.miR.21.post.12.months - df$Biosensor.miR.21.post.6.months)
# df$bio486.diff.z6.12 <- scale(df$Biosensor.miR.486.post.12.months - df$Biosensor.miR.486.post.6.months)
# df$bio24.diff.z6.12 <- scale(df$Biosensor.miR.24.post.12.months - df$Biosensor.miR.24.post.6.months)
# df$bio30d.diff.z6.12 <- scale(df$Biosensor.miR.30d.post.12.months - df$Biosensor.miR.30d.post.6.months)


