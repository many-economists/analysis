library(data.table)
library(ggplot2)

set.seed(1000)
effects = data.table(id = 1:100, effect = runif(100))
# Add some outliers in case that's important
effects = rbind(effects,
                data.table(id = 101:102, effect = 1.5))

match = CJ(id1 = 1:102,
           id2 = 1:102)
match = match[id1 != id2]

makediffhist = function(mdat) {
  mdat = merge(mdat, effects[, .(id1 = id, effect1 = effect)], by = "id1")
  mdat = merge(mdat, effects[, .(id2 = id, effect2 = effect)], by = "id2")
  mdat[, diff := abs(effect1 - effect2)]
  mdat[, difflev := cut(diff, breaks = c(-Inf, 0.1, 0.2, 0.3, 0.4,.5,.6,.7,.8,.9, Inf))]
  mdat[, N := .N]
  mdat = mdat[, .(Share = .N/first(N)), by = difflev]
  return(mdat)
}

orig = makediffhist(match)

# Now just do one match at a time
allruns = list() 
for (i in 1:400) {
  mcopy = copy(match)
  mcopy[, order := runif(.N)]
  setorder(mcopy, id1, order)
  mcopy = mcopy[, .(id2 = first(id2)), by = id1]
  allruns[[i]] = makediffhist(mcopy)
}

allruns = rbindlist(allruns)
allrunscombine = allruns[, .(Share = mean(Share)), by = difflev]


all = rbind(orig[, .(difflev, Share, Type = "Original")],
            allrunscombine[, .(difflev, Share, Type = "Random")])
ggplot(all, aes(x = difflev, y = Share, fill = Type)) +
  geom_col(stat = "identity", position = "dodge")


dat = import("../code/cleaned_survey_post_corrections.parquet", setclass = 'data.table')
dat = dat[!is.na(Revision_of_Q4) & Q2 == 'The first replication task', .(Q1, Revision_of_Q4)]

pairs = fread(paste0('../code/task_1_peer_review_pairs.csv'))
dat = dat[Q1 %in% pairs[(dont_send)]$id1]
effects = data.table(id = 1:nrow(dat),
                     effect = dat$Revision_of_Q4)

match = CJ(id1 = 1:nrow(dat),
           id2 = 1:nrow(dat))
match = match[id1 != id2]


makediffhist = function(mdat) {
  mdat = merge(mdat, effects[, .(id1 = id, effect1 = effect)], by = "id1")
  mdat = merge(mdat, effects[, .(id2 = id, effect2 = effect)], by = "id2")
  mdat[, diff := abs(effect1 - effect2)]
  mdat[, difflev := cut(diff, breaks = c(-Inf, .01, .025, 0.05, .1, .15, .2, .25, .3, Inf))]
  mdat[, N := .N]
  mdat = mdat[, .(Share = .N), by = difflev]
  return(mdat)
}

orig = makediffhist(match)
orig[, Share := Share/sum(Share)]

# Now just do one match at a time
allruns = list() 
for (i in 1:400) {
  mcopy = copy(match)
  mcopy[, order := runif(.N)]
  setorder(mcopy, id1, order)
  mcopy = mcopy[, .(id2 = first(id2)), by = id1]
  allruns[[i]] = makediffhist(mcopy)
}

allruns = rbindlist(allruns)
allrunscombine = allruns[, .(Share = sum(Share)/sum(allruns$Share)), by = difflev]


all = rbind(orig[, .(difflev, Share, Type = "Original")],
            allrunscombine[, .(difflev, Share, Type = "Random")])
ggplot(all, aes(x = difflev, y = Share, fill = Type)) +
  geom_col(stat = "identity", position = "dodge")
