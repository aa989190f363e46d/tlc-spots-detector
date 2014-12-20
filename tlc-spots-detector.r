library(magrittr)
library(EBImage)

plate.gray <- readImage('tlc-detector-example.tiff')
# инверсия. нужна потому что пятна, обычно, темнее фона
# и без этого пришлось бы постоянно контроллировать знаки в сравнениях
i.pl.gr = 1 - plate.gray
# занулить все кроме наибольших десяти перцентилей
# теоретически должно соответствовать +-3*СКО
i.pl.gr[i.pl.gr < quantile(imageData(i.pl.gr),0.925)] = 0

i.pl.gr %>% 
  channel(mode = 'gray') %>%   
  closing(makeBrush(size = 5,shape = 'gaussian',sigma = 0.5)) %T>%
  display(method = 'raster') %>% {parent.env(.BaseNamespaceEnv)$result <- .}

segments <- bwlabel(result)
area.ttl <- prod(dim(plate.gray))

areas.rl <- sapply(seq(max(segments)),function(i){sum(segments == i)}) / area.ttl
# убираем все пятна с относительной площадью меньше пороговой
 
i.pl.gr[!segments %in% which(areas.rl >= 3e-3)] = 0
display(i.pl.gr,method = 'raster')
