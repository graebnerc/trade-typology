# Figure 5: Trade Balance----------------------------------------------------

fig_trade_balance_abs <- ggplot(
  macro_data_agg,
  aes(
    x = year,
    y = ext_balance_gdp_fn1,
    color = cluster
  )
) +
  geom_ribbon(
    aes(
      ymin = ext_balance_gdp_fn1 - 0.5 * ext_balance_gdp_fn2,
      ymax = ext_balance_gdp_fn1 + 0.5 * ext_balance_gdp_fn2,
      fill = cluster
    ),
    alpha = 0.5, color = NA
  ) +
  geom_line() +
  geom_point()

fig_trade_balance_abs <- pretty_up_ggplot(fig_trade_balance_abs) +
  ggtitle("External Balance in % of GDP (1995-2017)") +
  scale_y_continuous(
    limits = c(-20, 35),
    breaks = seq(-20, 30, 5),
    labels = scales::percent_format(accuracy = 1, scale = 1)
  ) +
  scale_fill_manual(
    limits = names(unlist(cluster_cols)),
    values = c(unlist(cluster_cols)),
    aesthetics = c("fill", "color")
  ) +
  scale_x_continuous(
    limits = c(1995, 2017),
    breaks = seq(1995, 2015, 5),
    expand = expand_scale(
      c(0, 0),
      c(0, 0.25)
    )
  ) +
  theme(
    axis.title = element_blank()
  )

fig_trade_balance_abs

fig_trade_balance_cum <- ggplot(macro_data_cumulated) +
  geom_bar(aes(
    x = reorder(iso3c, TB_cum),
    y = TB_cum,
    fill = cluster, color = cluster
  ),
  stat = "identity"
  ) +
  ggtitle("Average Trade Balance in % of GDP (1995-2017)") +
  ylab("Average Trade Balance") +
  scale_x_discrete(
    limits = c(
      arrange(filter(
        macro_data_cumulated,
        iso3c %in% clustering[["Periphery"]]
      ), TB_cum)$iso3c,
      arrange(filter(
        macro_data_cumulated,
        iso3c %in% clustering[["Primary goods model"]]
      ), TB_cum)$iso3c,
      arrange(filter(
        macro_data_cumulated,
        iso3c %in% clustering[["Flexible labor markets model"]]
      ), TB_cum)$iso3c,
      arrange(filter(
        macro_data_cumulated,
        iso3c %in% clustering[["Industrial workbench model"]]
      ), TB_cum)$iso3c,
      arrange(filter(
        macro_data_cumulated,
        iso3c %in% clustering[["High tech model"]]
      ), TB_cum)$iso3c,
      arrange(filter(
        macro_data_cumulated,
        iso3c %in% clustering[["Financel hub"]]
      ), TB_cum)$iso3c
    )
  ) +
  scale_y_continuous(
    limits = c(-10, 27),
    breaks = seq(-10, 25, 5),
    labels = scales::percent_format(accuracy = 1, scale = 1)
  ) +
  scale_fill_manual(
    limits = names(unlist(cluster_cols)),
    values = c(unlist(cluster_cols)),
    aesthetics = c("fill", "color")
  ) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.line = element_line(),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 7),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.spacing.x = unit(0.2, "cm")
  )
fig_trade_balance_cum

fig_trade_balance_full <- ggpubr::ggarrange(
  fig_trade_balance_cum, fig_trade_balance_abs,
  ncol = 2, nrow = 1, common.legend = T, legend = "bottom",
  labels = c("A)", "B)"), font.label = list(face = "bold")
)

ggsave(
  filename = here("output/fig_5_trade-balance.pdf"),
  width = fig_width * 1.5, height = fig_height
)

fig_CA_TB_full <- ggpubr::ggarrange(
  fig_current_account_cum, fig_current_account_abs,
  fig_trade_balance_cum, fig_trade_balance_abs,
  ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom",
  labels = c("A)", "B)", "C)", "D)"), font.label = list(face = "bold")
)
