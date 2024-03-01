library(performance)
library(pscl)


sol.zip <- zeroinfl(Soleolifera ~ Treatment | Treatment, dist = "poisson", link = "logit", data = reveg)
r2_zeroinflated(sol.zip)


model.p = glm(Monarchs ~ Garden,
              data=Data,
              family="poisson")