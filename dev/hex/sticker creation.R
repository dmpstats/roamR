library(hexSticker)

library(showtext)
## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Orbitron", "orbi")
font_add_google("Audiowide", "audiowide")
font_add_google("Joti One", "joti")
font_add_google("Slackey", "slackey")
font_add_google("Chicle", "chicle")
font_add_google("Titan One", "titan")
font_add_google("Quantico", "quantico")
font_add_google("Zen Dots", "zen")
font_add_google("Jockey One", "jockey")
font_add_google("Goldman", "goldman")
font_add_google("Chewy", "chewy")
font_add_google("DinaPuff", "dinapuff")
font_add_google("Galindo", "galindo")



## Automatically use showtext to render text for future devices
showtext_auto()


# Option 1 ---------------------------------------------

sticker_opt1_a <- sticker("dev/hex/logo-original-no-bg.PNG",
                           package="roamR", p_size = 23, p_x = 1.33, p_y = 1.32, p_color = "black", p_fontface = "bold",
                           s_x = 1, s_y = 1, s_width = 0.85, s_height = 0.85,
                           #url = "https://dmpstats.github.io/roamR/", u_size = 5, u_color = "gray",
                           h_color = NA, h_fill = NA, h_size = 0,
                           dpi = 400,
                           filename = "dev/hex/roamR_sticker_opt1_a.png")


sticker_opt1_a

#usethis::use_logo("dev/hex/roamR_logo_sticker_2.png")


sticker_opt1_b <- sticker(
  "dev/hex/logo-original-no-bg.PNG",
  package="roamR", p_size = 25, p_x = 1.3, p_y = 1.38, p_color = "gray12", p_family = "orbi", #p_fontface = "bold",
  s_x = 1, s_y = 1, s_width = 0.85, s_height = 0.85,
  #url = "https://dmpstats.github.io/roamR/", u_size = 5, u_color = "gray",
  h_color = NA, h_fill = NA, h_size = 0,
  dpi = 400,
  filename = "dev/hex/roamR_sticker_opt1_b.png")


sticker_opt1_b

#usethis::use_logo("dev/hex/roamR_logo_sticker_3.png")

sticker_opt1_c <- sticker(
  "dev/hex/logo-original-no-bg.PNG",
  package="roamR", p_size = 25, p_x = 1.3, p_y = 1.38, p_color = "gray12", p_family = "orbi", p_fontface = "bold",
  s_x = 1, s_y = 1, s_width = 0.85, s_height = 0.85,
  #url = "https://dmpstats.github.io/roamR/", u_size = 5, u_color = "gray",
  h_color = NA, h_fill = NA, h_size = 0,
  dpi = 400,
  filename = "dev/hex/roamR_sticker_opt1_c.png")


sticker_opt1_c
#usethis::use_logo("dev/hex/roamR_sticker_opt1_c.png")



sticker_opt1_d <- sticker(
  "dev/hex/logo-original-no-bg.PNG",
  package = "",
  #p_size = 23, p_x = 1, p_y = 1.67, p_color = "gray95", p_family = "audiowide", p_fontface = "bold",
  s_x = 0.95, s_y = .93, s_width = 0.82, s_height = 0.82,
  url = "roamR", u_size = 25, u_color = "gray20", u_family = "audiowide", u_angle = 329,
  #u_x = 1.1, u_y = 0.52,
  u_x = 1.01, u_y = 1.9,
  h_color = NA, h_fill = NA,
  dpi = 400,
  filename = "dev/hex/roamR_sticker_opt1_d.png")


sticker_opt1_d <- sticker(
  "dev/hex/logo-original-no-bg.PNG",
  package = "",
  #p_size = 23, p_x = 1, p_y = 1.67, p_color = "gray95", p_family = "audiowide", p_fontface = "bold",
  s_x = 1.02, s_y = 1.1, s_width = 0.82, s_height = 0.82,
  url = "roamR", u_size = 25, u_color = "gray20", u_family = "audiowide", u_angle = 329,
  u_x = 0.19, u_y = 0.58,
  h_color = NA, h_fill = NA,
  dpi = 400,
  filename = "dev/hex/roamR_sticker_opt1_d.png")



sticker_opt1_d
#usethis::use_logo("dev/hex/roamR_sticker_opt1_c.png")



# Option 2 ---------------------------------------------

sticker_opt2_a <- sticker(
  "dev/hex/logo_option2_no-bg.PNG",
  package = "",
  #p_size = 25, p_x = 1.3, p_y = 1.38, p_color = "gray12", p_family = "orbi", p_fontface = "bold",
  s_x = 1, s_y = 1, s_width = 0.87, s_height = 0.87,
  url = "roamR", u_size = 23, u_color = "gray15", u_family = "audiowide",
  #u_x = 1.1, u_y = 0.52,
  u_x = 0.32, u_y = 1.37, u_angle = 23,
  h_color = NA, h_fill = NA, h_size = 0,
  dpi = 400,
  filename = "dev/hex/roamR_sticker_opt2_a.png")


sticker_opt2_a

#usethis::use_logo("dev/hex/roamR_logo_sticker_option2.png")




# Option 3 ---------------------------------------------

sticker_opt3_a <- sticker(
  "dev/hex/logo_option3_no-bg.PNG",
  package = "roamR",
  p_size = 23, p_x = 1, p_y = 1.67, p_color = "gray95", p_family = "audiowide", p_fontface = "bold",
  s_x = 1.01, s_y = 0.82, s_width = 0.71, s_height = 0.71,
  #url = "roamR", u_size = 22, u_color = "white", u_family = "audiowide", u_angle = 315,
  #u_x = 1.1, u_y = 0.52,
  #u_x = 0.32, u_y = 1.31,
  h_color = "black", h_fill = "gray25", h_size = 0.2,
  dpi = 400,
  filename = "dev/hex/roamR_sticker_opt3_a.png")



sticker_opt3_a

#usethis::use_logo("dev/hex/roamR_sticker_opt3_a.png")




sticker_opt3_b <- sticker(
  "dev/hex/logo_option3_no-bg.PNG",
  package = "",
  #p_size = 23, p_x = 1, p_y = 1.67, p_color = "gray95", p_family = "audiowide", p_fontface = "bold",
  s_x = 1.02, s_y = 1.1, s_width = 0.82, s_height = 0.82,
  url = "roamR", u_size = 25, u_color = "gray30", u_family = "goldman", u_angle = 329,
  u_x = 0.19, u_y = 0.58,
  h_color = NA, h_fill = NA,
  dpi = 400,
  filename = "dev/hex/roamR_sticker_opt3_b.png")


sticker_opt3_b
# usethis::use_logo("dev/hex/roamR_sticker_opt3_b.png")



sticker_opt3_c <- sticker(
  "dev/hex/logo_option3_no-bg_edited.PNG",
  package = "",
  #p_size = 23, p_x = 1, p_y = 1.67, p_color = "gray95", p_family = "audiowide", p_fontface = "bold",
  s_x = 1.02, s_y = 1.1, s_width = 0.89, s_height = 0.89,
  url = "roamR", u_size = 25, u_color = "gray50", u_family = "goldman", u_angle = 328,
  u_x = 0.19, u_y = 0.59,
  h_color = NA, h_fill = NA,
  dpi = 400,
  filename = "dev/hex/roamR_sticker_opt3_c.png")


sticker_opt3_c
# usethis::use_logo("dev/hex/roamR_sticker_opt3_c.png")





# Option 4 ---------------------------------------------

sticker_opt4_a <- sticker(
  "dev/hex/logo_option4_no-bg.PNG",
  package = "",
  #p_size = 23, p_x = 1, p_y = 1.67, p_color = "gray95", p_family = "audiowide", p_fontface = "bold",
  s_x = 1, s_y = 1, s_width = 0.97, s_height = 0.97,
  url = "roamR", u_size = 25, u_color = "white", u_family = "titan", u_angle = 31,
  #u_x = 1.1, u_y = 0.52,
  u_x = 01, u_y = 0.17,
  h_color = NA, h_fill = NA,
  dpi = 400,
  filename = "dev/hex/roamR_sticker_opt4_a.png")



sticker_opt4_a



# Option 5 ---------------------------------------------

sticker_opt5_a <- sticker(
  "dev/hex/logo_option5_no-bg.PNG",
  package = "",
  #p_size = 23, p_x = 1, p_y = 1.67, p_color = "gray95", p_family = "audiowide", p_fontface = "bold",
  s_x = 0.97, s_y = 1, s_width = 0.92, s_height = 0.92,
  url = "roamR", u_size = 35, u_color = "white", u_family = "jockey", u_angle = 32,
  #u_x = 1.1, u_y = 0.52,
  u_x = 0.95, u_y = 0.29,
  h_color = NA, h_fill = NA,
  dpi = 400,
  filename = "dev/hex/roamR_sticker_opt5_a.png")



sticker_opt5_a



# Option 6 ---------------------------------------------

sticker_opt6_a <- sticker(
  "dev/hex/logo_option6_no-bg.PNG",
  package = "",
  #p_size = 23, p_x = 1, p_y = 1.67, p_color = "gray95", p_family = "audiowide", p_fontface = "bold",
  s_x = 1.09, s_y = 0.92, s_width = 0.88, s_height = 0.88,
  url = "roamR", u_size = 26, u_color = "gray50", u_family = "goldman", u_angle = 30,
  #u_x = 1.1, u_y = 0.52,
  u_x = 0.27, u_y = 1.43,
  h_color = NA, h_fill = NA,
  dpi = 400,
  filename = "dev/hex/roamR_sticker_opt6_a.png")




sticker_opt6_a

usethis::use_logo("dev/hex/roamR_sticker_opt6_a.png")




sticker_opt6_b <- sticker(
  "dev/hex/logo_option6_no-bg.PNG",
  package = "",
  #p_size = 23, p_x = 1, p_y = 1.67, p_color = "gray95", p_family = "audiowide", p_fontface = "bold",
  s_x = 1.09, s_y = 0.92, s_width = 0.88, s_height = 0.88,
  url = "roamR", u_size = 26, u_color = "gray85", u_family = "goldman", u_angle = 30,
  #u_x = 1.1, u_y = 0.52,
  u_x = 0.27, u_y = 1.43,
  h_color = NA, h_fill = NA,
  dpi = 400,
  filename = "dev/hex/roamR_sticker_opt6_b.png")



sticker_opt6_b





# Option 7 ---------------------------------------------

sticker_opt7_a <- sticker(
  "dev/hex/logo_option7_no-bg.PNG",
  package = "",
  #p_size = 23, p_x = 1, p_y = 1.67, p_color = "gray95", p_family = "audiowide", p_fontface = "bold",
  s_x = 1.02, s_y = 1.1, s_width = 0.82, s_height = 0.82,
  url = "roamR", u_size = 25, u_color = "gray20", u_family = "galindo", u_angle = 326,
  u_x = 0.26, u_y = 0.65,
  h_color = NA, h_fill = NA,
  dpi = 400,
  filename = "dev/hex/roamR_sticker_opt7_a.png")



sticker_opt7_a
