# # filename <- here::here("data/70kHz_FM.xml")
# filename <- here::here("data/120kHz_FM.xml")
# # filename <- here::here("data/120kHz_FM_25mm sphere.xml")
# # filename <- here::here("data/200kHz_FM_25mm sphere_PartsCombined.xml")
# vessel.name <- "Lasker"
# cal.group <- "SWFSC"
# #
# library(patchwork)
# library(tidyverse)
# #
# tmp <- extract_cal_fm(filename, vessel.name, survey.name = "Test")$cal.res
#
# p1 <- ggplot(tmp, aes(freq, gain)) +
#   geom_line() +
#   geom_point(shape = 21, fill = "white", size = 1) +
#   ylab("Gain (dB)") + xlab("Frequency (kHz)") +
#   theme_bw()
#
# p2 <- ggplot() +
#   geom_line(data = tmp, aes(freq, ba.alon), colour = "cyan") +
#   geom_line(data = tmp, aes(freq, ba.athw), colour = "magenta") +
#   geom_point(data = tmp, aes(freq, ba.alon),
#              shape = 21, fill = "white", colour = "cyan", size = 1) +
#   geom_point(data = tmp, aes(freq, ba.athw),
#              shape = 21, fill = "white", colour = "magenta", size = 1) +
#   ylab("Beamwidth (deg)") + xlab("Frequency (kHz)") +
#   theme_bw()
#
# p4 <- ggplot() +
#   geom_line(data = tmp, aes(freq, oa.alon), colour = "cyan") +
#   geom_line(data = tmp, aes(freq, oa.athw), colour = "magenta") +
#   geom_point(data = tmp, aes(freq, oa.alon),
#              shape = 21, fill = "white", colour = "cyan", size = 1) +
#   geom_point(data = tmp, aes(freq, oa.athw),
#              shape = 21, fill = "white", colour = "magenta", size = 1) +
#   ylab("Beam offset (deg)") + xlab("Frequency (kHz)") +
#   theme_bw()
#
# # library(patchwork)
#
#
# p.all <- p1 / p2 / p4
#
# ggsave(p.all, filename = "tmp.png")
