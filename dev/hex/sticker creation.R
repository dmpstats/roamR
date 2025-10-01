library(hexSticker)

roamR_sticker <- sticker("dev/hex/logo-original-no-bg.PNG",
              package="roamR", p_size = 21, p_x = 1, p_y = 1.55, p_color = "black", #p_fontface = "bold",
              s_x = 1, s_y = 1, s_width = 0.9, s_height = 0.9,
              #url = "https://dmpstats.github.io/roamR/", u_size = 5, u_color = "gray",
              h_color = NA, h_fill = NA, h_size = 0,
              dpi = 400,
              filename = "dev/hex/roamR_logo_sticker.png")


roamR_sticker

#usethis::use_logo("dev/hex/roamR_logo_sticker.png")




roamR_sticker_2 <- sticker("dev/hex/logo-original-no-bg.PNG",
                           package="roamR", p_size = 23, p_x = 1.33, p_y = 1.32, p_color = "black", p_fontface = "bold",
                           s_x = 1, s_y = 1, s_width = 0.85, s_height = 0.85,
                           #url = "https://dmpstats.github.io/roamR/", u_size = 5, u_color = "gray",
                           h_color = NA, h_fill = NA, h_size = 0,
                           dpi = 400,
                           filename = "dev/hex/roamR_logo_sticker_2.png")


roamR_sticker_2

#usethis::use_logo("dev/hex/roamR_logo_sticker_2.png")





library(showtext)
## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Orbitron", "orbi")
font_add_google("Audiowide", "audiowide")


## Automatically use showtext to render text for future devices
showtext_auto()



roamR_sticker_3 <- sticker(
  "dev/hex/logo-original-no-bg.PNG",
  package="roamR", p_size = 25, p_x = 1.3, p_y = 1.38, p_color = "gray12", p_family = "orbi", #p_fontface = "bold",
  s_x = 1, s_y = 1, s_width = 0.85, s_height = 0.85,
  #url = "https://dmpstats.github.io/roamR/", u_size = 5, u_color = "gray",
  h_color = NA, h_fill = NA, h_size = 0,
  dpi = 400,
  filename = "dev/hex/roamR_logo_sticker_3.png")


roamR_sticker_3
#usethis::use_logo("dev/hex/roamR_logo_sticker_3.png")

roamR_sticker_4 <- sticker(
  "dev/hex/logo-original-no-bg.PNG",
  package="roamR", p_size = 25, p_x = 1.3, p_y = 1.38, p_color = "gray12", p_family = "orbi", p_fontface = "bold",
  s_x = 1, s_y = 1, s_width = 0.85, s_height = 0.85,
  #url = "https://dmpstats.github.io/roamR/", u_size = 5, u_color = "gray",
  h_color = NA, h_fill = NA, h_size = 0,
  dpi = 400,
  filename = "dev/hex/roamR_logo_sticker_4.png")


roamR_sticker_4
usethis::use_logo("dev/hex/roamR_logo_sticker_4.png")

