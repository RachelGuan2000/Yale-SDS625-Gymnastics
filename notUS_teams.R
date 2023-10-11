# not US teams
top5attempt2 <- function(data, country, gender) {
  data %>%
  filter(Country == country, Gender == gender) %>%
  mutate(Apparatus = ifelse(Apparatus == "VT1", "VT", Apparatus)) %>%
  mutate(Apparatus = ifelse(Apparatus == "VT2", "VT", Apparatus)) %>%
  group_by(Apparatus) %>%
  arrange(desc(pred), .by_group = TRUE) %>%
  top_n(1, pred)
}

top5_m_try2 <- lapply(top_countries_m$Country, top5attempt2, data = gymnast_score_predicts, gender ="m")

top5_m_CHN <- top5_m_try2[[1]]
top5_m_IRI <- top5_m_try2[[2]]
top5_m_ARM <- top5_m_try2[[3]]
top5_m_PHI <- top5_m_try2[[4]]
top5_m_ALB <- top5_m_try2[[5]]
top5_m_ENG <- top5_m_try2[[6]]
top5_m_NIR <- top5_m_try2[[7]]
top5_m_JPN <- top5_m_try2[[8]]
top5_m_JOR <- top5_m_try2[[9]]
top5_m_CCS <- top5_m_try2[[10]]
top5_m_VIE <- top5_m_try2[[11]]
top5_m_UKR <- top5_m_try2[[12]]



top5_w_try2 <- lapply(top_countries_w$Country, top5attempt2, data = gymnast_score_predicts, gender ="w")

top5_m_ITA <- top5_m_try2[[1]]
top5_m_CHN <- top5_m_try2[[2]]
top5_m_ENG <- top5_m_try2[[3]]
top5_m_GER <- top5_m_try2[[4]]
top5_m_MEX <- top5_m_try2[[5]]
top5_m_NED <- top5_m_try2[[6]]
top5_m_FRA <- top5_m_try2[[7]]
top5_m_ROU <- top5_m_try2[[8]]
top5_m_HUN <- top5_m_try2[[9]]
top5_m_ESP <- top5_m_try2[[10]]
top5_m_AUS <- top5_m_try2[[11]]
top5_m_JPN <- top5_m_try2[[12]]


top12m_all <- rbind(top5_m_CHN,
                    top5_m_IRI,
                    top5_m_ARM,
                    top5_m_PHI,
                    top5_m_ALB,
                    top5_m_ENG,
                    top5_m_NIR,
                    top5_m_JPN,
                    top5_m_JOR,
                    top5_m_CCS,
                    top5_m_VIE,
                    top5_m_UKR)

top12w_all <- rbind(top5_m_ITA,
                    top5_m_CHN,
                    top5_m_ENG,
                    top5_m_GER,
                    top5_m_MEX,
                    top5_m_NED,
                    top5_m_FRA,
                    top5_m_ROU,
                    top5_m_HUN,
                    top5_m_ESP,
                    top5_m_AUS,
                    top5_m_JPN)
