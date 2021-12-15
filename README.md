---
title: "Cauchy Distribution"
output: 
  html_document:
    keep_md: TRUE
---



概念

![](fig/Cauchy.png)

平均が収束しない理由: ゼロに近づくのと同じ程度、∞に近づくから

![](fig/Cauchy_mean.png)


ライブラリ読み込み


```r
library(tidyverse)
library(patchwork)
```

サンプリング


```r
set.seed(71)

N <- 5000

df_cauchy <-
  tibble(N = 1:N,
         theta = runif(N, 0, 2*pi)) %>%
  mutate(val = 1 / tan(theta / 2)) %>% 
  mutate(mean = cumsum(val) / N) %>% 
  mutate(x = sin(theta),
         y = cos(theta)) %>% 
  mutate(xend = 0,
         yend = 1)
```

単位円描画用のデータフレーム


```r
en <-
  tibble(theta = seq(0, 2 * pi, length = 1000)) %>% 
  mutate(x = cos(theta),
         y = sin(theta))
```

描画


```r
.alpha <- 0.05  # あれこれの透明度
.l <- 6         # 軸範囲（±）

i <- 5000　　　 # 描画するサンプル番号

# フィルタ
dat_i <-
  df_cauchy %>% 
  filter(N <= i)

# 単位円と3点の描画
g_plot <-
  dat_i %>% 
  ggplot() +
  geom_hline(yintercept = 0, color = "darkgrey") +
  geom_vline(xintercept = 0, color = "darkgrey") +
  geom_vline(data = df_cauchy %>% filter(N == i),
             aes(xintercept = val), 
             color = "red", linetype = "dotted") +
  geom_path(data = en,
            aes(x = x, y = y), 
            alpha  = 0.5) +
  geom_path(data = en %>% filter(theta <= df_cauchy$theta[i]),
            aes(x = y, y = x), 
            color = "blue", size = 0.5) +
  geom_segment(data = df_cauchy %>% filter(N == i),
               aes(x = x, y = y),
               yend = 0, xend = 0, 
               color = "skyblue") +
  geom_segment(data = df_cauchy %>% filter(N == i),
               aes(x = x, y = y),
               yend = 1, xend = 0, 
               color = "pink") +
  geom_segment(data = df_cauchy %>% filter(N == i),
               aes(x = x / abs(x) * .l, 
                   y = 1 - (1 - y) / abs(x) * .l),
               yend = 1, xend = 0, 
               color = "pink") +
  geom_segment(data = df_cauchy %>% filter(N == i),
               aes(x = val),
               y = 0, yend = 1, xend = 0, 
               color = "pink") +
  geom_point(x = 0, y = 1) +
  geom_point(aes(x = val, y = 0), 
             color = "red", alpha = .alpha, size = 0.5) +
  geom_point(data = df_cauchy %>% filter(N == i),
             aes(x = x, y = y), 
             color = "blue") +
  geom_point(data = df_cauchy %>% filter(N == i),
             aes(x = val, y = 0), 
             color = "red", size = 1.5) +
  geom_text(data = df_cauchy %>% filter(N == i),
            aes(label = str_c("N=", N)),
            x = -.l, y = 1, hjust = 0, vjust = 1) +
  scale_x_continuous(limits = c(-.l, .l)) +
  scale_y_continuous(limits = c(-1, 1)) +
  theme_classic() +
  coord_fixed()

# thetaの確率密度分布
g_dens_u <-
  dat_i %>% 
  ggplot() +
  aes(theta) +
  geom_density(color = "blue", fill = "skyblue") +
  geom_vline(data = dat_i %>% filter(N == i),
             aes(xintercept = theta),
             color = "blue", linetype = "dotted") +
  geom_point(aes(x = theta, y = 0), 
             color = "blue", alpha = .alpha, size = 0.5) +
  geom_point(data = dat_i %>% filter(N == i),
             aes(x = theta, y = 0), 
             color = "blue", size = 1) +
  scale_x_continuous(limits = c(0, 2* pi),
                     breaks = c(0, pi, 2 * pi),
                     labels = c("0", "pi", "2pi")) +
  scale_y_continuous(limits = c(0, NA),
                     expand = c(0.1, 0)) +
  theme_classic() +
  xlab("theta") +
  ylab("density")

# xの確率密度分布
g_dens <-
  dat_i %>% 
  ggplot() +
  aes(val) +
  geom_density(color = "red", fill = "pink") +
  geom_vline(xintercept = 0, color = "darkgrey") +
  geom_point(aes(x = val, y = 0), 
             color = "red", alpha = .alpha) +
  geom_vline(data = dat_i %>% filter(N == i),
             aes(xintercept = val),
             color = "red", linetype = "dotted") +
  geom_vline(data = dat_i %>% filter(N == i),
             aes(xintercept = mean),
             color = "red", size = 1) +
  scale_x_continuous(limits = c(-.l, .l)) +
  scale_y_continuous(limits = c(0, NA),
                     expand = c(0.1, 0)) +
  theme_classic() +
  theme(axis.title.x = element_blank()) +
  ylab("density")

# xの平均の推移
g_mean <-
  dat_i %>% 
  ggplot() +
  aes(mean, N) +
  geom_vline(xintercept = 0, color = "darkgrey") +
  geom_path(color = "red", size = 1) +
  scale_x_continuous(limits = c(-.l, .l)) +
  theme_classic()
```

図の合成


```r
g <-
  wrap_plots(g_dens_u, g_plot, g_dens, g_mean,
             heights = c(0.5, 0.7, 1, 1))
```

保存


```r
ggsave("fig/cauchydist.png", g, width = 5, height = 6)
```


![](fig/cauchydist.png)

補足: 確率密度関数

![](fig/pdf.png)



