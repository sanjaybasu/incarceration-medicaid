#Health outcome
# Mostly to do with code for state-level analysis (sample size too small)

#survey glm by location

# aggStats.loc <- svyglm(formula = reformulate(c(aggVarsShort,aggVars[["state"]]),
#                                              response = "opioidUse_any"),
#                        design = spi_svy,
#                        family="quasibinomial")
# aggStats.loc <- svyglm(formula = opioidUse_any ~ RV0002 * raceEthnicity * RV0005 + V0772,
#                        design = spi_svy,
#                        family="quasibinomial")
#
# aggStats.coefs <- summary(aggStats.loc)$coefficients %>%
#   as.data.frame() %>%
#   mutate(ymin = Estimate - `Std. Error`*1.96, ymax = Estimate + `Std. Error`*1.96) %>%
#   rownames_to_column("Coefficient")
#
#
# df <- aggStats.coefs %>% filter(grepl("V0772", Coefficient)) %>%
#   mutate(State = gsub("V0772","", Coefficient)) %>%
#   arrange(Estimate)
# df$State <- factor(df$State, levels = df$State)
#
# ggplot(df,
#        aes(x = State, y = Estimate, ymin = ymin, ymax = ymax)) +
#   geom_point() + geom_errorbar() +   coord_cartesian(ylim=c(-2, 2))
#
# pred.x <- expand.grid(c(sapply(unname(aggVarsShort), function(x) na.omit(unique(spiData2[[x]]))),
#                         list(V0772 = na.omit(unique(spiData2[[aggVars[["state"]]]])))))
#
# pred.y <- predict(aggStats.loc, newdata = pred.x, type="link", se.fit = TRUE) %>% as.data.frame()
#
# mod.invlink = aggStats.loc$family$linkinv
# preds <- list(m = mod.invlink(pred.y$link),
#               upr = mod.invlink(pred.y$link + (qnorm(0.975) * pred.y$SE)),
#               lwr = mod.invlink(pred.y$link - (qnorm(0.975) * pred.y$SE))
#               ) %>% as.data.frame()
#
# aggVarsAll <- c(aggVarsShort, State = aggVars[["state"]])
#
# preds <- pred.x %>%
#   rename_with(.fn = function(x) names(aggVarsAll)[aggVarsAll == x]) %>%
#   bind_cols(preds)
#
# library(stringr)
# levels(preds$ageCat) <- unname(sapply(levels(preds$ageCat),
#                                              function(x) strsplit(x, "= ")[[1]][2]))
# levels(preds$sex) <- unname(sapply(levels(preds$sex),
#                                           function(x) strsplit(x, "= ")[[1]][2]))
# levels(preds$raceEthnicity) <- unname(sapply(levels(preds$raceEthnicity),
#                                                     function(x) {
#                                                       if(grepl("=",x)) strsplit(x, "= ")[[1]][2]
#                                                       else x}))
# preds$raceEthnicity <- factor(preds$raceEthnicity,
#                                      levels = c("Hispanic","Black (NH)","White (NH)","Multiracial, other, or missing"))
#
# plotStates <- function(stateValues, sexValue = "Male"){
#   ggplot(preds %>% filter(State %in% stateValues, sex == sexValue),
#          aes(x = ageCat,
#              y = m, ymin = lwr, ymax = upr, # linetype = sex, alpha = sex,
#              color = raceEthnicity, fill = raceEthnicity, group = raceEthnicity)) +
#     geom_ribbon(alpha = 0.5) +
#     geom_line() +
#     scale_y_continuous("Population (%)", labels = scales::percent, expand = c(0,0)) +
#     scale_x_discrete("Age", guide = guide_axis(angle = 45)) +
#     # scale_alpha_discrete(range = c(0.25, 0.9)) +
#     theme_bw() +
#     facet_wrap("State", ncol = 5)
# }
#
# stateValuesAll <- levels(preds$State)
# lapply(split(stateValuesAll, ceiling(seq_along(stateValuesAll)/26)), function(x) plotStates(x, "Male"))
#
# ggsave(filename, g, width = 10, height = 10)

#tedious, but the svyciprop can't do it all at once. the CI's for svymean is incorrect (outside of 0 to 1)
# aggStats <- svyby(colForm,
#                   by=aggForm,
#                   design = spi_svy,
#                   FUN = svymean,
#                   vartype="ci",
#                   method="beta",
#                   keep.names = F
# )
#
# colnames(aggStats) <- c(names(aggVarsShort),
#                         paste0(names(colVars),".m"),
#                         paste0(names(colVars),".lwr"),
#                         paste0(names(colVars),".upr"))
#
# library(ggplot2)
# aggNames <- names(aggVarsShort)
# aggStatsLongMean <- aggStats %>%
#   select(-ends_with(c(".lwr",".upr"))) %>%
#   pivot_longer(ends_with(".m"),
#                names_to = "health_outcome",
#                values_to = "mean") %>%
#   mutate(health_outcome = gsub("\\.m","",health_outcome))
#
# aggStatsLongLwr <- aggStats %>%
#   select(!!aggNames, ends_with(".lwr")) %>%
#   pivot_longer(ends_with(".lwr"),
#                names_to = "health_outcome",
#                values_to = "lwr") %>%
#   mutate(health_outcome = gsub("\\.lwr","",health_outcome))
#
# aggStatsLongUpr <- aggStats %>%
#   select(!!aggNames, ends_with(".upr")) %>%
#   pivot_longer(ends_with(".upr"),
#                names_to = "health_outcome",
#                values_to = "upr") %>%
#   mutate(health_outcome = gsub("\\.upr","",health_outcome))
#
# aggStatsLong <- aggStatsLongMean %>%
#   left_join(aggStatsLongLwr) %>%
#   left_join(aggStatsLongUpr)