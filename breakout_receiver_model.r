# PACKAGES ------
# library(tidyverse)
# library(scales)
# library(janitor)
library(gt)
library(gtExtras)
# library(glue)
# library(httr)
# library(rvest)
# library(nflplotR)
library(ggthemes)
source("./own_model/R/env_details.R")


# VALUES ----
# USER_AGENT = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Safari/537.36 OPR/101.0.0.0"
# token_4for4 = "Drupal.visitor.subscriptionPlan=betting_early_bird_2023; Drupal.visitor.drupal_login=a%3A5%3A%7Bs%3A16%3A%22SESSION_USERNAME%22%3Bs%3A11%3A%22Matt%20Savoca%22%3Bs%3A12%3A%22SESSION_TYPE%22%3Bi%3A1%3Bs%3A16%3A%22SESSION_LOGGEDIN%22%3Bi%3A1%3Bs%3A19%3A%22SESSION_LOGINFAILED%22%3Bi%3A0%3Bs%3A11%3A%22FULL_IMPACT%22%3Bi%3A1%3B%7D; f4__ESPNLeagueSyncExtensionWrongBrowser=true; SSESSc40475ee089b0d4e8158fa797261c63c=suG-bAxpbt6KUK8DzJIpotF3XpXiBxPlsMhqT59vVmY; ff4for4uid=113767; full-impact_default_page_league=%7B%22full-impact_xfactor_60444%22%3A%22244780%22%2C%22full-impact_auction-values_60444%22%3A%22183605%22%7D; ffdr_default_page_league=%7B%22ffdr%22%3A%2260444%22%2C%22ffdr_rankings_183605%22%3A%22183605%22%2C%22ffdr_half-ppr-rankings%22%3A%2260444%22%2C%22ffdr_ppr-rankings%22%3A%2260444%22%2C%22ffdr_fanduel-rankings%22%3A%2260446%22%2C%22ffdr_rankings_243841%22%3A%22243841%22%7D; site=draftkings; f4__Slatesdraftkings=%7B%22slate_id%22%3A5754%7D; __stripe_mid=fc66c145-86c8-45ec-b2ff-6d0a4fd2bb83bfc48e; reports_default_page_league=%7B%22reports_redraft_cheat_sheet%22%3A%22244780%22%2C%22reports_redraft_cheat_sheet_244780%22%3A%22244780%22%2C%22reports_redraft_cheat_sheet_252296%22%3A%22252296%22%7D; Drupal.visitor.cover_blocks:selected_tab=dfs; user_mode=dfs; ffr_default_page_league=%7B%22ffr%22%3A%2260444%22%2C%22ffr_fanduel-rankings%22%3A%2260446%22%2C%22ffr_draftkings-rankings%22%3A%2291309%22%2C%22ffr_half-ppr-rankings%22%3A%22244780%22%2C%22ffr_rankings_183605%22%3A%22183605%22%2C%22ffr_ppr-rankings%22%3A%2238353%22%2C%22ffr_rankings_244780%22%3A%22244780%22%2C%22ffr_rankings_245647%22%3A%22244780%22%7D; aucp13n=bqojul"
# player_db_path = "https://docs.google.com/spreadsheets/d/e/2PACX-1vRCdg3bjY0eCmqWlYdJQWGhp58ml3Ewl8n20WdRQs93SFPQ2XsHMi2duRdGc2R0gIhGWDTPgMPLeqZp/pub?gid=1815226455&single=true&output=csv"
L1_EFF_CUTOFF <- 0.95 # IF PLAYER'S LAST 1 WEEK SCORE IS WITHIN _% OF EXPECTATIONS, LEAVE OFF THE BUY-LOW
L3_EFF_CUTOFF <- 0.95 # IF PLAYER'S LAST 3 WEEK SCORE IS WITHIN _% OF EXPECTATIONS, LEAVE OFF THE BUY-LOW
theme_set(theme_few())

# WEEKLY UPDATES ----
# dotenv::load_dot_env(".env")
# token_4for4 = Sys.getenv("cookie_4for4")
# CURRENT_WEEK = 17
XFP_CUTOFF <- 6
player_db <- player_db_final
BYES <- c("")
DK_CODE <- dk_csv_id
FD_CODE <- fd_csv_id
injured <- c("")


# WEEKLY DATASETS ------
mainslate_csv_dk_raw <- GET(
    glue("https://www.4for4.com/dfs_projections_csv/{DK_CODE}/0/89510"),
    add_headers(
        Cookie = glue("{token_4for4}"),
        `User-Agent` = USER_AGENT
    )
) %>%
    content(as = "parsed") %>%
    clean_names()

mainslate_csv_fd_raw <- GET(
    glue("https://www.4for4.com/dfs_projections_csv/{FD_CODE}/0/89510"),
    add_headers(
        Cookie = glue("{token_4for4}"),
        `User-Agent` = USER_AGENT
    )
) %>%
    content(as = "parsed") %>%
    clean_names()


scores_4for4_raw <- read_csv(glue("https://www.4for4.com/tools/stat-app-csv/ff_points_half_ppr/{CURRENT_SEASON}/1/{CURRENT_WEEK-1}/ALL/ALL"))



# FUNCTIONS ----

# parse_4for4_proj = function(scoring, player_db = player_db_final, season, week){
#   scraped = scrape_4for4_proj_weekly(format = scoring, week = week, season = season)

#   joined = scraped %>%
#     left_join(player_db %>% select(pid = player_id, fantasypros_id)) %>%
#     left_join(player_db %>% select(pid = player, dst_pid = player_id, dst_fp_id = fantasypros_id)) %>%
#     mutate(
#       across(contains("_id"), ~as.character(.x)),
#       pid = if_else(pos == "DST", dst_pid, pid),
#       team = if_else(pos == "DST", dst_pid, team),
#       fantasypros_id = if_else(pos == "DST", dst_fp_id, fantasypros_id),
#     ) %>%
#     select(-contains("dst_"))

#   return(joined)
# }

# scrape_4for4_proj_weekly = function(
#     format = "dk", week  = CURRENT_WEEK, season = CURRENT_SEASON){


#   dst_df_raw  = scrape_4for4_dst_proj_weekly(format = format)

#   dst_df  = dst_df_raw %>%
#     select(pid = team_defense, team = team_defense, player = team_defense, ff_pts) %>%
#     mutate(week = week, season = season, pos = "DST")


#   sflex_df_raw  = scrape_4for4_sflex_proj_weekly(format = format)

#   sflex_df = sflex_df_raw %>%
#     select(pid, player, pos, team, week, season, ff_pts)

#   rbind(
#     sflex_df,
#     dst_df
#   )
# }

# scrape_4for4_dst_proj_weekly = function(format = "fd", token = token_4for4) {
#   scrape_site = if(tolower(format) == "fd"){"fanduel"}else{"draftkings"}


#   # Initialize
#   url = glue("http://www.4for4.com/fantasy-football-rankings/{scrape_site}-rankings/def")

#   # Make a GET request with the cookie
#   response = GET(
#     url,
#     add_headers(
#       Cookie = glue('{token_4for4}')
#     ))



#   content = content(response, "text")

#   scraped_data = read_html(content) %>%
#     html_nodes("#sortable-rankings-table_wrapper") %>%  # Notice the "#" indicating it's an ID
#     html_table(header = TRUE)

#   # Safety check
#   if (length(scraped_data[[1]]) >= 1) {
#     data = scraped_data[[1]] %>% clean_names()
#   } else {
#     stop("Even with a cookie, no dice. Check class or token.")
#   }

#   df = data

#   return(data)
# }


# scrape_4for4_sflex_proj_weekly = function(format = "dk"){

#   if(!format %in% c("fd", "dk")){
#     print("INVALID FORMAT! CHOOSE ONE OF: 'fd', 'dk'")
#     return(NULL)
#   }else{
#     page = case_when(
#       format == "fd" ~ "60446",
#       T ~ "91309"
#     )

#     proj_4for4_raw <- GET(
#       glue('https://www.4for4.com/projections_weekly_csv/{page}'),

#       add_headers(
#         Cookie = glue('{token_4for4}'),
#         `User-Agent` = USER_AGENT
#       )
#     )
#     proj_4for4 = proj_4for4_raw %>%
#       content(as = "parsed") %>%
#       clean_names()
#   }
# }


# inseason_name_updates_4for4 = function(nm){
#   name = gsub("Devon Achane", "De'Von Achane", nm)
#   name = gsub("Joshua Palmer", "Josh Palmer", name)
#   return(name)
# }




# CODE -------
fd_pricing_raw <- mainslate_csv_fd_raw
dk_pricing_raw <- mainslate_csv_dk_raw
last_week_model_raw <- read_csv(glue("./Week {CURRENT_WEEK-1}/model.csv"))
model_raw <- read_csv(glue("./Week {CURRENT_WEEK}/model.csv"))
player_db <- player_db_final
fd_proj_raw <- scrape_4for4_weekly_projections_via_api(api_key_4for4) %>%
    filter(!position %in% c("DEF", "QB")) %>%
    select(pid = player_id, team, fd_proj = ff_pts_half) %>%
    mutate(fd_proj = parse_number(fd_proj))

NO_LASTWEEK_MODEL <- FALSE


# TABLES --------
scores_4for4 <- scores_4for4_raw %>%
    clean_names() %>%
    select(-starts_with("x0")) %>%
    pivot_longer(cols = starts_with("w"), names_to = "week", values_to = "fpts") %>%
    mutate(
        week = parse_number(week),
        fpts = parse_number(fpts),
        # player = inseason_name_updates_4for4(player)
    ) %>%
    filter(!is.na(fpts)) %>%
    left_join(
        player_db %>% select(player, pid = player_id)
    ) %>%
    filter(!is.na(pid))

last_week_model_df <- last_week_model_raw %>%
    clean_names() %>%
    left_join(
        player_db %>% select(name = breakout_model_name, pid = player_id, fantasypros_id, gsis_id)
    ) %>%
    left_join(scores_4for4 %>%
        filter(week >= (CURRENT_WEEK - 1)) %>% group_by(pid) %>% transmute(
            team,
            pid,
            week_score = max(week),
            week_fpts = mean(fpts, na.rm = T)
        ) %>%
        ungroup()) %>%
    mutate(
        l1_eff_score = week_fpts / l3_x_pts,
        l3_eff_score = l3_f_pts / l3_x_pts,
        resid = week_fpts - l3_x_pts,
        hit = if_else(l1_eff_score >= L1_EFF_CUTOFF, 1, 0)
    ) %>%
    distinct()

last_week_model_df %>%
    select(name, pos, team,
        xfp = l3_x_pts,
        fp = week_fpts,
        hit,
        `prev +/-` = diff,
        `this_week +/-` = resid
    ) %>%
    filter(hit == 1, `prev +/-` < 0, (xfp > XFP_CUTOFF | fp > XFP_CUTOFF), ) %>%
    arrange(-xfp) %>%
    select(-hit) %>%
    gt() %>%
    gt::tab_header(title = glue("Week {CURRENT_WEEK - 1} Hits")) %>%
    gtExtras::gt_theme_espn() %>%
    data_color(columns = `this_week +/-`, palette = "PRGn") %>%
    fmt_number(
        columns = contains(c("fp", "`prev +/-`f", "`this_week +/-`")),
        decimals = 1
    ) %>%
    gt::gtsave(filename = glue("./Week {CURRENT_WEEK}/last_week_hits.html"), inline_css = T)

model_strength_df <- last_week_model_df %>%
    mutate(
        l3_x_pts = round(l3_x_pts)
    ) %>%
    group_by(l3_x_pts) %>%
    summarize(
        n = n(),
        week_fpts = mean(week_fpts, na.rm = T),
        .groups = "drop"
    )

lm_summary <- summary(lm(week_fpts ~ l3_x_pts, data = model_strength_df))

lm_summary$r.squared

if (NO_LASTWEEK_MODEL == F) {
    model_df <- model_raw %>%
        clean_names() %>%
        left_join(
            player_db %>% select(name = breakout_model_name, pid = player_id, fantasypros_id, gsis_id)
        ) %>%
        left_join(scores_4for4 %>% filter(week >= (CURRENT_WEEK - 1)) %>% select(pid, l1_f_pts = fpts)) %>%
        left_join(
            fd_pricing_raw %>% clean_names() %>% select(pid, fd, opp, grade)
        ) %>%
        left_join(
            dk_pricing_raw %>% clean_names() %>% select(pid, dk)
        ) %>%
        left_join(
            fd_proj_raw %>% select(pid, team, fd_proj)
        ) %>%
        left_join(
            last_week_model_df %>% select(pid, last_week_hit = hit)
        ) %>%
        mutate(
            last_week_hit = replace_na(last_week_hit, 0),
            grade = str_sub(grade, 1, 1),
            p10_diff = p10_x_pts - p10_f_pts,
            l1_eff_score = round(l1_f_pts / l3_x_pts, 2),
            l3_eff_score = round(l3_f_pts / l3_x_pts, 2),
            dk_x_pts_1k_l3 = l3_x_pts / (dk / 1000),
            fd_x_pts_1k_l3 = l3_x_pts / (fd / 1000),
            p10_efficiency = case_when(
                p10_efficiency > 200 ~ 200,
                T ~ p10_efficiency
            ),
            p10_score = rescale(p10_efficiency, to = c(-1, 1)),
            p10_score_label = case_when(
                is.na(p10_score) ~ "",
                p10_score >= .67 ~ "A+",
                p10_score >= .33 ~ "A",
                p10_score >= -.33 & p10_score < .33 ~ "B",
                p10_score <= -.67 ~ "D",
                T ~ "C"
            ),
            across(where(is.numeric), ~ round(.x, 1))
        )
}

model_df_no_lastweek <- model_raw %>%
    clean_names() %>%
    left_join(
        player_db %>% select(name = breakout_model_name, pid = player_id, fantasypros_id, gsis_id)
    ) %>%
    left_join(scores_4for4 %>% filter(week >= (CURRENT_WEEK - 1)) %>% select(pid, l1_f_pts = fpts)) %>%
    left_join(
        fd_pricing_raw %>% clean_names() %>% select(pid, fd, opp, grade)
    ) %>%
    left_join(
        dk_pricing_raw %>% clean_names() %>% select(pid, dk)
    ) %>%
    left_join(
        fd_proj_raw %>% select(pid, team, fd_proj)
    ) %>%
    # left_join(last_week_model_df %>% select(pid, last_week_hit = hit)) %>%
    mutate(
        last_week_hit = 0,
        grade = str_sub(grade, 1, 1),
        p10_diff = p10_x_pts - p10_f_pts,
        l1_eff_score = round(l1_f_pts / l3_x_pts, 2),
        l3_eff_score = round(l3_f_pts / l3_x_pts, 2),
        dk_x_pts_1k_l3 = l3_x_pts / (dk / 1000),
        fd_x_pts_1k_l3 = l3_x_pts / (fd / 1000),
        p10_efficiency = case_when(
            p10_efficiency > 200 ~ 200,
            T ~ p10_efficiency
        ),
        p10_score = rescale(p10_efficiency, to = c(-1, 1)),
        p10_score_label = case_when(
            is.na(p10_score) ~ "",
            p10_score >= .67 ~ "A+",
            p10_score >= .33 ~ "A",
            p10_score >= -.33 & p10_score < .33 ~ "B",
            p10_score <= -.67 ~ "D",
            T ~ "C"
        ),
        across(where(is.numeric), ~ round(.x, 1))
    )


if (NO_LASTWEEK_MODEL) {
    model_df <- model_df_no_lastweek
}

breakout_model_chart_df <- model_df %>%
    # mutate(name = if_else(last_week_hit == 1, paste0(name, "*"), name)) %>%
    filter(
        diff < 0, !name %in% injured, l3_eff_score < L3_EFF_CUTOFF, last_week_hit != 1, # l1_eff_score < L1_EFF_CUTOFF,
    )

values_df <- model_df %>%
    filter(
        !name %in% injured
    )


dfs_buylows <- values_df %>%
    filter(!is.na(dk) & !is.na(fd)) %>%
    arrange(
        -fd, -fd_x_pts_1k_l3
    ) %>%
    mutate(
        l3_x_pts = as.numeric(l3_x_pts),
        fd_proj = as.numeric(fd_proj),
        fd = scales::dollar(fd),
        dk = scales::dollar(dk),
        proj_vs_xfp = fd_proj - l3_x_pts
    ) %>%
    select(
        Player = name,
        Pos = pos,
        Team = team,
        Opp = opp,
        `Matchup Grade` = grade,
        `Fanduel Salary` = fd,
        `DraftKings Salary` = dk,
        `Expected HPPR FPts` = l3_x_pts,
        `Projected vs. Expected FPts` = proj_vs_xfp,
        `FD Opportunity Value` = fd_x_pts_1k_l3,
        `DK Opportunity Value` = dk_x_pts_1k_l3,
        l3_eff_score, l1_eff_score, fd_proj, diff, l3_f_pts, last_week_hit
    ) %>%
    filter(
        fd_proj >= 6, diff < 0, l3_eff_score < L3_EFF_CUTOFF, last_week_hit != 1, # l1_eff_score < L1_EFF_CUTOFF,
    ) %>%
    select(-l3_eff_score, -diff, -last_week_hit) %>%
    mutate(
        across(contains("Value"), ~ as.numeric(.x)),
        across(where(is.numeric), ~ round(.x, 1))
    )

# CHARTS -------
model_strength_chart <- model_strength_df %>%
    ggplot() +
    aes(
        x = l3_x_pts,
        y = week_fpts,
        size = n
    ) +
    geom_point() +
    geom_smooth(lty = 2, method = "glm", color = "black", show.legend = F) +
    labs(
        x = "Mean Expected FPTs, Last 3 Weeks",
        y = "Mean Observed FPts, Last 3 Weeks",
        size = "Observations",
    ) +
    scale_x_continuous(breaks = pretty_breaks()) +
    geom_label(
        label = paste0("~R^{2}: ", round(lm_summary$r.squared, 2)[1]),
        x = 15, y = 4, size = 6,
        parse = T
    ) +
    labs(
        title = "4for4 Breakout Receiver Model Strength",
        subtitle = "Grouped by Expected FPts",
        caption = "Chart: Matt Savoca @draftaholic | Data: @4for4Football"
    )

buylow_plot <- breakout_model_chart_df %>%
    filter(fd_proj >= 4, l3_x_pts > XFP_CUTOFF) %>%
    # filter(!team %in% BYES) %>%
    arrange(
        -l3_x_pts
    ) %>%
    mutate(player = reorder(name, l3_x_pts)) %>%
    ggplot() +
    aes(
        y = player, fill = team,
    ) +
    geom_col(aes(x = l3_x_pts), alpha = .5) +
    geom_col(aes(x = l3_f_pts), color = "black", width = .3) +
    geom_nfl_logos(aes(team_abbr = team), x = -0.4, width = 0.035) +
    scale_fill_nfl() +
    labs(
        x = "HPPR Fantasy Points, Expected (Faint Bar) vs. Observed (Solid)",
        y = "",
        title = glue("Receiver Breakout Model, Week {CURRENT_WEEK}"),
        subtitle = "Players Performing Below Expectation, Last 3 Weeks",
        caption = "Chart: Matt Savoca @draftaholic | Data: @4for4Football"
    ) +
    theme(
        axis.text = element_text(face = "bold", size = 13)
    )


sellhigh_plot <- values_df %>%
    filter(fd_proj >= 4, l3_f_pts > XFP_CUTOFF, l3_eff_score > 1, diff > 0) %>%
    arrange(
        -l3_f_pts
    ) %>%
    mutate(player = reorder(name, l3_f_pts)) %>%
    ggplot() +
    aes(
        y = player, fill = team,
    ) +
    geom_col(aes(x = l3_x_pts), alpha = .5) +
    geom_col(aes(x = l3_f_pts), color = "black", width = .3) +
    geom_nfl_logos(aes(team_abbr = team), x = -0.4, width = 0.035) +
    scale_fill_nfl() +
    labs(
        x = "HPPR Fantasy Points, Expected (Faint Bar) vs. Observed (Solid)",
        y = "",
        title = glue("Receiver Breakout Model, Week {CURRENT_WEEK}"),
        subtitle = "Players Performing Above Expectation, Last 3 Weeks",
        caption = "Chart: Matt Savoca @draftaholic | Data: @4for4Football"
    ) +
    theme(
        axis.text = element_text(face = "bold", size = 13)
    )


dfs_buylows_chart <- dfs_buylows %>%
    arrange(-fd_proj) %>%
    mutate(
        proj_value = fd_proj / (parse_number(`Fanduel Salary`) / 1000),
        player_label = glue("{Player} ({`Fanduel Salary`})"),
        Player = reorder(player_label, proj_value)
    ) %>%
    ggplot() +
    aes(
        y = Player, fill = Team,
    ) +
    geom_col(aes(x = `FD Opportunity Value`), alpha = .5) +
    geom_col(aes(x = proj_value), fill = "black", width = .3) +
    geom_nfl_logos(aes(team_abbr = Team), x = -0.4, width = 0.035) +
    scale_fill_nfl() +
    labs(
        x = "FanDuel Value, Expected (Faint Bar) vs. Projected (Solid)",
        y = "",
        title = glue("Fanduel Breakout Candidates, Week {CURRENT_WEEK}"),
        caption = "Chart: Matt Savoca @draftaholic | Data: @4for4Football"
    ) +
    theme(
        axis.text = element_text(face = "bold", size = 9)
    )


dfs_breakouts <- dfs_buylows %>%
    arrange(-fd_proj) %>%
    mutate(
        proj_value = fd_proj / (parse_number(`Fanduel Salary`) / 1000),
        player_label = glue("{Player} ({`Fanduel Salary`})"),
        Player = reorder(player_label, `FD Opportunity Value`)
    ) %>%
    ggplot() +
    aes(
        y = Player, fill = Team,
    ) +
    geom_col(aes(x = `FD Opportunity Value`), alpha = .5) +
    geom_col(aes(x = proj_value), fill = "black", width = .3) +
    geom_nfl_logos(aes(team_abbr = Team), x = -0.4, width = 0.035) +
    scale_fill_nfl() +
    labs(
        x = "FanDuel Value, Expected (Faint Bar) vs. Projected (Solid)",
        y = "",
        title = glue("DFS Breakout Candidates, Week {CURRENT_WEEK}"),
        caption = "Chart: Matt Savoca @draftaholic | Data: @4for4Football"
    ) +
    theme(
        axis.text = element_text(face = "bold", size = 9)
    )


# EXPORTS ----
ggsave(plot = sellhigh_plot, filename = glue("./Week {CURRENT_WEEK}/sellhigh_plot.png"), width = 650 * 4, height = 650 * 4.3, units = "px")
ggsave(plot = buylow_plot, filename = glue("./Week {CURRENT_WEEK}/buylow_plot.png"), width = 650 * 4, height = 650 * 4.3, units = "px")
ggsave(plot = dfs_breakouts, filename = glue("./Week {CURRENT_WEEK}/dfs_breakouts.png"), width = 650 * 4, height = 650 * 4.3, units = "px")
ggsave(plot = model_strength_chart, filename = glue("./Week {CURRENT_WEEK}/model_strength_chart.png"), width = 650 * 4, height = 650 * 4.3, units = "px")
ggsave(plot = dfs_buylows_chart, filename = glue("./Week {CURRENT_WEEK}/model_strength_chart.png"), width = 650 * 4, height = 650 * 4.3, units = "px")


breakout_model_chart_df %>%
    filter(fd_proj >= 4, l3_x_pts > XFP_CUTOFF) %>%
    arrange(
        -l3_x_pts
    ) %>%
    mutate(player = reorder(name, l3_x_pts)) %>%
    select(
        Player = player,
        Pos = pos,
        Team = team,
        `Expected HPPR FPts` = l3_x_pts,
        `FPts` = l3_f_pts,
        `FPts vs. Expected` = diff,
        `Efficiency Grade` = p10_score_label
    ) %>%
    write_csv(glue("./Week {CURRENT_WEEK}/breakout_model_buylow.csv"))


values_df %>%
    filter(fd_proj >= 4, l3_f_pts > XFP_CUTOFF, l3_eff_score > 1, diff > 0) %>%
    arrange(
        -l3_f_pts
    ) %>%
    select(
        Player = name,
        Pos = pos,
        Team = team,
        `Expected HPPR FPts` = l3_x_pts,
        `FPts` = l3_f_pts,
        `FPts vs. Expected` = diff,
        `Efficiency Grade` = p10_score_label
    ) %>%
    write_csv(glue("./Week {CURRENT_WEEK}/breakout_model_sellhigh.csv"))


model_df %>%
    arrange(-l3_x_pts) %>%
    select(
        Player = name, `Pos.` = pos, `Team` = team,
        `Expected HPPR FPts` = l3_x_pts,
        `FPts` = l3_f_pts,
        `FPts vs. Expected` = diff,
    ) %>%
    write_csv(glue("./Week {CURRENT_WEEK}/xfp_model.csv"))

last_week_model_df %>%
    select(name, pos, team, xfp = l3_x_pts, fp = week_fpts, hit, `prev +/-` = diff, `this_week +/-` = resid) %>%
    filter(
        hit == 1,
        `prev +/-` < 0,
        (xfp > XFP_CUTOFF | fp > XFP_CUTOFF)
    ) %>%
    arrange(-xfp) %>%
    select(-hit) %>%
    rename(
        `Player` = name,
        `Pos` = pos,
        `Team` = team,
        `Expected HPPR FPts` = xfp,
        `FPts` = fp,
        `Prev. +/-` = `prev +/-`,
        `This Week +/-` = `this_week +/-`
    ) %>%
    mutate(across(where(is.numeric), ~ round(.x, 1))) %>%
    write_csv(glue("./Week {CURRENT_WEEK}/last_week_model_hits.csv"))

dfs_buylows %>%
    select(-fd_proj, -l3_f_pts, -Opp, -l1_eff_score) %>%
    mutate(across(where(is.numeric), ~ round(.x, 1))) %>%
    write_csv(glue("./Week {CURRENT_WEEK}/dfs_buylows.csv"))


message("Breakout Model Analysis Complete.")
