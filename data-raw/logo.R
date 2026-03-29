# Public domain images manually downloaded into `raw-data` from
# https://commons.wikimedia.org/wiki/Category:Tarot_nouveau_-_Grimaud_-_1898

library("affiner")
library("glue")
library("grid")
library("jpeg")
img_filename <- function(suit, rank) {
	stopifnot(suit %in% c("Clubs", "Diamonds", "Hearts", "Spades"), rank %in% c("Jack", "King"))
	glue::glue("raw-data/Tarot_nouveau_-_Grimaud_-_1898_-_{suit}_-_{rank}.jpeg")
}

grey <- "gainsboro"

img_j <- jpeg::readJPEG(img_filename("Clubs", "Jack"))
img_j <- img_j[90:1320, 110:(1507 - 109), ]
img_j <- as.raster(img_j)
img_j[1:100, 1:100] <- grey
img_j[1:100, 1289:(1289 - 100)] <- grey

img_k <- jpeg::readJPEG(img_filename("Spades", "King"))
img_k <- img_k[100:1330, 110:(1507 - 109), ]
img_k <- as.raster(img_k)
img_k[1:100, 1:100] <- grey
img_k[1:500, 1289:(1289 - 100)] <- grey

xy <- as_coord2d(angle(seq(90, 360 + 90, by = 60), "degrees"), radius = c(rep(0.488, 6), 0))
xy$translate(coord2d(x = 0.5, y = 0.5))

# xy_left <- xy[c(2, 3, 7, 1)]
xy_left <- xy[c(7, 4, 3, 2)]
left_settings <- affine_settings(xy_left, unit = "snpc")

# xy_right <- xy[c(1, 7, 5, 6)]
xy_right <- xy[c(6, 5, 4, 7)]
right_settings <- affine_settings(xy_right, unit = "snpc")

xy_top <- xy[c(1, 2, 7, 6)]
top_settings <- affine_settings(xy_top, unit = "snpc")

vp_define <- viewport(width = unit(3, "inches"), height = unit(3, "inches"))
grob_left <- affineGrob(
	rasterGrob(img_j),
	vp_define = vp_define,
	transform = left_settings$transform,
	vp_use = left_settings$vp
)
grob_right <- affineGrob(
	rasterGrob(img_k),
	vp_define = vp_define,
	transform = right_settings$transform,
	vp_use = right_settings$vp
)
grob_text <- grobTree(
	polygonGrob(x = c(0, 0, 1, 1), y = c(1, 0, 0, 1), gp = gpar(fill = grey, col = NA)),
	textGrob(
		"affiner",
		x = 0.42,
		y = 0.6,
		rot = 45,
		gp = gpar(fontfamily = "Pinyon Script", fontsize = 90)
	)
)
grob_top <- affineGrob(
	grob_text,
	vp_define = vp_define,
	transform = top_settings$transform,
	vp_use = top_settings$vp
)

gp_border <- gpar(fill = NA, col = "black", lwd = 12)

draw_logo <- function() {
	grid.draw(grob_left)
	grid.polygon(xy_left$x, xy_left$y, gp = gp_border)
	grid.draw(grob_right)
	grid.polygon(xy_right$x, xy_right$y, gp = gp_border)
	grid.draw(grob_top)
	grid.polygon(xy_top$x, xy_top$y, gp = gp_border)
}

w <- 4.5

png("man/figures/logo.png", width = w, height = w, units = "in", res = 72, bg = "transparent")
draw_logo()
dev.off()
