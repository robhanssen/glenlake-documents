library(tidyverse)
library(patchwork)
library(ggpmisc)

theme_set(theme_light() +
    theme(
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0)
    ))

resp <-
    read_csv("activities.csv") %>%
    janitor::clean_names() %>%
    filter(str_detect(action, "Survey response")) %>%
    arrange(date) %>%
    mutate(
        date = with_tz(date, "US/Eastern"),
        count = seq_len(nrow(.)),
    )

answer_1 <- "Closed 24/7"
answer_2 <- "Open during daytime"

results <-
    c(
        rep(answer_1, 36),
        rep(answer_2, 33)
    )


last_date <-
    last(resp$date) %>%
    format(., format = "%H:%M on %b %d")

time_plot <-
    ggplot(resp, aes(x = date, y = count)) +
    geom_point(shape = 1) +
    geom_line(
        # stat = "smooth", method = "loess",
        color = "gray70",
        alpha = .5
    ) +
    scale_y_continuous(
        limits = c(0, NA),
        labels = scales::label_number()
    ) +
    labs(
        x = "", y = "Response count (cumulative)",
        title = glue::glue("Total response to the survey: ", length(results)),
        caption = glue::glue("Data collected up to {last_date}")
    )

#
# analysis of results
#

chi <- chisq.test(
    table(results),
)

params <- broom::tidy(chi)

note_table <- as_tibble(table(results))

sign <- ifelse(params$p.value < .05, "", "not ")
pval <- scales::pvalue(params$p.value)

note <- glue::glue(
    "\U03A7^2 analysis shows that the difference is {sign}significant ",
    "(p", ifelse(str_detect(pval, "<"), "", "="), "{pval})"
)

note <-
    case_when(
        params$p.value > 0.05 ~
            glue::glue("No option is preferred over the other significantly ",
                "(p", ifelse(str_detect(pval, "<"), "", "="), "{pval}) "),
        sum(results == answer_1) > length(results == answer_2) ~
            glue::glue("There is a preference for {answer_1}",
            "(p", ifelse(str_detect(pval, "<"), "", "="), "{pval}) "
            ),
        sum(results == answer_1) < length(results == answer_2) ~
            glue::glue("There is a preference for {answer_2} ",
            "(p", ifelse(str_detect(pval, "<"), "", "="), "{pval})"
            ),
        TRUE ~
            ""
    )

mns <-
    map_dbl(
        seq_len(10000),
        ~ mean(sample(results,
            length(results),
            replace = TRUE
        ) == "Closed 24/7")
    )

bounds <-
    broom::tidy(
        binom.test(sum(results == "Closed 24/7"), length(results))
    )


analysis_g <-
    ggplot(data = tibble(m = mns), aes(x = m)) +
    geom_density(fill = "gray50", alpha = .5) +
    geom_vline(
        xintercept = .5,
        color = "gray20",
        linewidth = 2,
        alpha = .8
    ) +
    scale_x_continuous(
        labels = scales::percent_format(),
        limits = c(0, 1)
    ) +
    geom_errorbar(
        data = bounds,
        aes(xmin = conf.low, xmax = conf.high, y = .75), inherit.aes = FALSE,
        width = .3, linewidth = 2, alpha = .5, color = "gray20"
    ) +
    annotate("table",
        x = 0, y = 6,
        label = note_table, hjust = 0
    ) +
    annotate("label",
        x = c(0, 1), y = .75, vjust = 1, hjust = c(0, 1),
        label = c(answer_2, answer_1)
    ) +
    labs(
        x = "Vote proportion", y = "",
        title = note
    ) +
    theme(
        axis.text.y = element_blank()
    )

ggsave("vote_analysis.png",
    width = 12, height = 6,
    plot = time_plot + analysis_g
)
