head(sp@data)
spn <- data.frame(id = sp$id, district = sp$district,
                  aan = sp$notif.animals,
                  eaan = sp$notif.animals.expected,
                  shvn = sp$notif.humans)

head(spn)
spn[ , 3] <- spn[ , 3] + sample(1:2, 96, r = T)
spn[ , 5] <- scale(spn[ , 5] + sample(1:2, 96, r = T))

save(spn, sp.adj, file = 'data/sp.RData')

