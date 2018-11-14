# ----------------------------------------------------------------------------------- 
# ---- Packages ---------------------------------------------------------------------
# ----------------------------------------------------------------------------------- 

require(data.table)
require(foreach)
require(doParallel)
require(ggplot2)
require(scales)

# ----------------------------------------------------------------------------------- 
# ---- Input Parameters -------------------------------------------------------------
# ----------------------------------------------------------------------------------- 

{

# bundles is a data table that indicates which products belong to which bundles

bundles = data.table(bundle = c(1, 2, 3, rep(4, 2), rep(5, 2), rep(6, 2), rep(7, 3)), 
						product = c("A", "B", "C", "A", "B", "A", "C", "B", "C", "A", "B", "C"))

# demands is a data table that indicates the mean and std dev of each products demand, where all demands are independent and normally distributed
# unit of demand is items per day

demands = data.table(product = c("A", "B", "C"),
						norm.mean = c(100, 80, 50),
						norm.sd = c(20, 10, 5))

# leadtimes is a data.table that indicates:
	# the mean and std dev of each bundles leadtime, where all leadtimes are independent and normally distributed
	# the likelihood of interuption
	# the rate of interuption delay, where all interuption delays are independent and exponentially distributed
# unit of time is days
	
leadtimes = data.table(supplier = c(rep(1, 3), rep(2, 3), 3),
						bundle = 1:7,
						mean.norm = c(1, 1.25, 1.33, 2, 2.5, 2.25, 3),
						sd.norm = c(0.1, 0.2, 0.2, 0.3, 0.2, 0.3, 0.5),
						risk = c(rep(0.3, 3), rep(0.2, 3), 0.1),
						rate.exp = c(rep(1, 3), rep(0.75, 3), 0.5))

# samp is a parameter for the sample size used in the calculations

samp = 1000000

# digits is the number of decimal places that the service level function will be rounded to
# this rounding is to evaluate if we have found a root to the service level function which will give us our safety stock

digits = 6

# desired overall service level

a = 0.9

# initialize the safety stock weight
	# this is a starting value that will be optimized using newtons method for finding the root of a differentiable function

k.start = 1

}

# ----------------------------------------------------------------------------------- 
# ---- Build the Experiment ---------------------------------------------------------
# ----------------------------------------------------------------------------------- 

{

# in this experiment we will answer the following questions:
	# What happens as risk increases?
		# Increase for each supplier
		# Increase for all suppliers
	# What happens when the shortage delay length increases?
		# Increase for each supplier
		# Increase for all suppliers
	# What happens when lead time increases?
		# Increase for each supplier
		# Increase for all suppliers
	# What happens when demand for products increases?
		# Increase demand for each product
		# Increase demand for all
	# What happens when the variance of demand for products increases?
		# Increase demand for each product
		# Increase demand for all
	# What happens when the variance of lead time for bundles increases?
		# Increase for each supplier
		# Increase for all suppliers

# LOWER and UPPER are the lower and upper bounds of the range for which we will randomly increase various metrics
	# these metrics are:
		# product demand (mean)
		# product demand (standard deviation)
		# bundle leadtime (mean)
		# bundle leadtime (standard deviation)
		# bundle leadtime (risk)
		# bundle leadtime (rate)
	
LOWER = 0.25
UPPER = 0.75

# reps is the number of times to randomly sample from this range

reps = 30

# ---- increase the product demand (mean) -------------------------------------------

# inc.dem.mean randomly samples percent increases for the demand of each product across all scenarios
	# increase the demand for one product at a time
	# increase the demand for all products

inc.dem.mean = runif(n = reps * nrow(demands), min = LOWER, max = UPPER)
inc.dem.mean = unlist(lapply(1:(nrow(demands) + 1), function(i) inc.dem.mean))

# apply.dem.mean is a vector of zeros and ones to ensure that one product at a time experiences the increase

apply.dem.mean = foreach(i = 1:(nrow(demands) + 1)) %do%
{
	# if this is the last scenario than create a vector of all ones
	
	if(i == nrow(demands) + 1)
	{
		output = rep(1, nrow(demands) * reps)
	
	} else
	{
		# otherwise only place a 1 corresponding the i-th product
		
		output = rep(0, nrow(demands))
		output[i] = 1
		output = rep(output, reps)
	}
	
	return(output)
}

apply.dem.mean = unlist(apply.dem.mean)

# multiply inc.dem.mean and apply.dem.mean together to get a vector that will:
	# increase the demand for one product at a time
	# increase the demand for all products

inc.dem.mean = apply.dem.mean * inc.dem.mean

rm(apply.dem.mean, i, output)

# ---- increase the product demand (standard deviation) -----------------------------

# inc.dem.sd randomly samples percent increases for the demand of each product across all scenarios
	# increase the demand for one product at a time
	# increase the demand for all products

inc.dem.sd = runif(n = reps * nrow(demands), min = LOWER, max = UPPER)
inc.dem.sd = unlist(lapply(1:(nrow(demands) + 1), function(i) inc.dem.sd))

# apply.dem.sd is a vector of zeros and ones to ensure that one product at a time experiences the increase

apply.dem.sd = foreach(i = 1:(nrow(demands) + 1)) %do%
{
	# if this is the last scenario than create a vector of all ones
	
	if(i == nrow(demands) + 1)
	{
		output = rep(1, nrow(demands) * reps)
	
	} else
	{
		# otherwise only place a 1 corresponding the i-th product
		
		output = rep(0, nrow(demands))
		output[i] = 1
		output = rep(output, reps)
	}
	
	return(output)
}

apply.dem.sd = unlist(apply.dem.sd)

# multiply inc.dem.sd and apply.dem.sd together to get a vector that will:
	# increase the demand for one product at a time
	# increase the demand for all products

inc.dem.sd = apply.dem.sd * inc.dem.sd

rm(apply.dem.sd, i, output)

# ---- increase the bundle leadtime (mean) -------------------------------------------

# inc.lead.mean randomly samples percent increases for the leadtime of each supplier across all scenarios
	# increase the leadtime for one supplier at a time
	# increase the leadtime for all suppliers

inc.lead.mean = runif(n = reps * nrow(leadtimes), min = LOWER, max = UPPER)
inc.lead.mean = unlist(lapply(1:(max(leadtimes$supplier) + 1), function(i) inc.lead.mean))

# apply.lead.mean is a vector of zeros and ones to ensure that one supplier at a time experiences the increase

apply.lead.mean = foreach(i = 1:(max(leadtimes$supplier) + 1)) %do%
{
	# if this is the last scenario than create a vector of all ones
	
	if(i == max(leadtimes$supplier) + 1)
	{
		output = rep(1, nrow(leadtimes) * reps)
	
	} else
	{
		# otherwise only place a 1 corresponding the i-th supplier
		
		output = rep(0, nrow(leadtimes))
		output[which(leadtimes$supplier == i)] = 1
		output = rep(output, reps)
	}
	
	return(output)
}

apply.lead.mean = unlist(apply.lead.mean)

# multiply inc.lead.mean and apply.lead.mean together to get a vector that will:
	# increase the leadtime for one supplier at a time
	# increase the leadtime for all suppliers

inc.lead.mean = apply.lead.mean * inc.lead.mean

rm(apply.lead.mean, i, output)

# ---- increase the bundle leadtime (standard deviation) ----------------------------

# inc.lead.sd randomly samples percent increases for the leadtime of each supplier across all scenarios
	# increase the leadtime for one supplier at a time
	# increase the leadtime for all suppliers

inc.lead.sd = runif(n = reps * nrow(leadtimes), min = LOWER, max = UPPER)
inc.lead.sd = unlist(lapply(1:(max(leadtimes$supplier) + 1), function(i) inc.lead.sd))

# apply.lead.sd is a vector of zeros and ones to ensure that one supplier at a time experiences the increase

apply.lead.sd = foreach(i = 1:(max(leadtimes$supplier) + 1)) %do%
{
	# if this is the last scenario than create a vector of all ones
	
	if(i == max(leadtimes$supplier) + 1)
	{
		output = rep(1, nrow(leadtimes) * reps)
	
	} else
	{
		# otherwise only place a 1 corresponding the i-th supplier
		
		output = rep(0, nrow(leadtimes))
		output[which(leadtimes$supplier == i)] = 1
		output = rep(output, reps)
	}
	
	return(output)
}

apply.lead.sd = unlist(apply.lead.sd)

# multiply inc.lead.sd and apply.lead.sd together to get a vector that will:
	# increase the leadtime for one supplier at a time
	# increase the leadtime for all suppliers

inc.lead.sd = apply.lead.sd * inc.lead.sd

rm(apply.lead.sd, i, output)

# ---- increase the bundle leadtime (risk) ------------------------------------------

# inc.lead.risk randomly samples percent increases for the leadtime of each supplier across all scenarios
	# increase the leadtime for one supplier at a time
	# increase the leadtime for all suppliers

inc.lead.risk = runif(n = reps * nrow(leadtimes), min = LOWER, max = UPPER)
inc.lead.risk = unlist(lapply(1:(max(leadtimes$supplier) + 1), function(i) inc.lead.risk))

# apply.lead.risk is a vector of zeros and ones to ensure that one supplier at a time experiences the increase

apply.lead.risk = foreach(i = 1:(max(leadtimes$supplier) + 1)) %do%
{
	# if this is the last scenario than create a vector of all ones
	
	if(i == max(leadtimes$supplier) + 1)
	{
		output = rep(1, nrow(leadtimes) * reps)
	
	} else
	{
		# otherwise only place a 1 corresponding the i-th supplier
		
		output = rep(0, nrow(leadtimes))
		output[which(leadtimes$supplier == i)] = 1
		output = rep(output, reps)
	}
	
	return(output)
}

apply.lead.risk = unlist(apply.lead.risk)

# multiply inc.lead.risk and apply.lead.risk together to get a vector that will:
	# increase the leadtime for one supplier at a time
	# increase the leadtime for all suppliers

inc.lead.risk = apply.lead.risk * inc.lead.risk

rm(apply.lead.risk, i, output)

# ---- increase the bundle leadtime (rate) ------------------------------------------

# inc.lead.rate randomly samples percent increases for the leadtime of each supplier across all scenarios
	# increase the leadtime for one supplier at a time
	# increase the leadtime for all suppliers

inc.lead.rate = runif(n = reps * nrow(leadtimes), min = LOWER, max = UPPER)
inc.lead.rate = unlist(lapply(1:(max(leadtimes$supplier) + 1), function(i) inc.lead.rate))

# apply.lead.rate is a vector of zeros and ones to ensure that one supplier at a time experiences the increase

apply.lead.rate = foreach(i = 1:(max(leadtimes$supplier) + 1)) %do%
{
	# if this is the last scenario than create a vector of all ones
	
	if(i == max(leadtimes$supplier) + 1)
	{
		output = rep(1, nrow(leadtimes) * reps)
	
	} else
	{
		# otherwise only place a 1 corresponding the i-th supplier
		
		output = rep(0, nrow(leadtimes))
		output[which(leadtimes$supplier == i)] = 1
		output = rep(output, reps)
	}
	
	return(output)
}

apply.lead.rate = unlist(apply.lead.rate)

# multiply inc.lead.rate and apply.lead.rate together to get a vector that will:
	# increase the leadtime for one supplier at a time
	# increase the leadtime for all suppliers

inc.lead.rate = apply.lead.rate * inc.lead.rate

rm(apply.lead.rate, i, output)

# ---- build a demand and leadtime table for each scenario --------------------------

# ---- demand table -------------------------------

# metrics is the number of metrics we are increasing for each products demand
	# in the case of increasing the mean and sd of each product --> metrics = 2

metrics = 2

# dem.scenario is the scenario id numbers 

dem.scenario = unlist(lapply(1:((nrow(demands) + 1) * metrics), function(i) rep(i, reps * nrow(demands))))
dem.scenario = c(rep(1, nrow(demands)), dem.scenario + 1)

# dem.replication is the 

dem.replication = unlist(lapply(1:((nrow(demands) + 1) * metrics), function(i) sort(rep(1:reps, nrow(demands)))))
dem.replication = c(rep(1, nrow(demands)), dem.replication)

# dem.table is the demand table whihc indicates by what percent to increase the mean and sd of each product demand in every scenario and replication

dem.table = data.table(scenario = dem.scenario, 
						replication = dem.replication,
						inc.mean = c(rep(0, nrow(demands)), inc.dem.mean, rep(0, length(dem.scenario) - length(inc.dem.mean) - nrow(demands))),
						inc.sd = c(rep(0, length(dem.scenario) - length(inc.dem.sd)), inc.dem.sd))

rm(dem.scenario, dem.replication, metrics, inc.dem.mean, inc.dem.sd)

# ---- leadtime table -----------------------------

# metrics is the number of metrics we are increasing for each suppliers leadtime
	# in the case of increasing the mean, sd, risk, and rate of each supplier --> metrics = 4

metrics = 4

# lead.scenario is the scenario id numbers 

lead.scenario = unlist(lapply(1:((max(leadtimes$supplier) + 1) * metrics), function(i) rep(i, reps * nrow(leadtimes))))
lead.scenario = c(rep(1, nrow(leadtimes)), lead.scenario + max(dem.table$scenario))

# lead.replication is the replication id numbers

lead.replication = unlist(lapply(1:((max(leadtimes$supplier) + 1) * metrics), function(i) sort(rep(1:reps, nrow(leadtimes)))))
lead.replication = c(rep(1, nrow(leadtimes)), lead.replication)

# lead.table is the demand table which indicates by what percent to increase the mean, sd, risk, and rate of each supplier leadtime in every scenario and replication

lead.table = data.table(scenario = lead.scenario, 
						replication = lead.replication,
						inc.mean = c(rep(0, nrow(leadtimes)), inc.lead.mean, rep(0, length(lead.scenario) - length(inc.lead.mean) - nrow(leadtimes))),
						inc.sd = c(rep(0, nrow(leadtimes)), rep(0, length(inc.lead.sd)), inc.lead.sd, rep(0, 2 * length(inc.lead.sd))),
						inc.risk = c(rep(0, nrow(leadtimes)), rep(0, 2 * length(inc.lead.risk)), inc.lead.risk, rep(0, length(inc.lead.risk))),
						inc.rate = c(rep(0, length(lead.scenario) - length(inc.lead.rate)), inc.lead.rate))

rm(lead.scenario, lead.replication, metrics, inc.lead.mean, inc.lead.sd, inc.lead.risk, inc.lead.rate)

# ---- update tables ------------------------------

# dem.scenario is the other scenario id numbers that correspond to leadtime increases

dem.scenario = unlist(lapply((max(dem.table$scenario) + 1):max(lead.table$scenario), function(i) rep(i, reps * nrow(demands))))

# dem.replication is the replication id numbers 

dem.replication = unlist(lapply((max(dem.table$scenario) + 1):max(lead.table$scenario), function(i) sort(rep(1:reps, nrow(demands)))))

# lead.scenario is the other scenario id numbers that correspond to demand increases

lead.scenario = unlist(lapply(2:max(dem.table$scenario), function(i) rep(i, reps * nrow(leadtimes))))

# lead.replication is the replication id numbers 

lead.replication = unlist(lapply(2:max(dem.table$scenario), function(i) sort(rep(1:reps, nrow(leadtimes)))))

# add dem.scenario and dem.replication to dem.table

DT = data.table(scenario = dem.scenario, 
				replication = dem.replication, 
				inc.mean = 0, 
				inc.sd = 0)

dem.table = rbind(dem.table, DT)

# add a product column so we can see which products face what increase

dem.table[, product := demands$product]

rm(dem.scenario, dem.replication, DT)

# add lead.scenario and lead.replication to lead.table

DT = data.table(scenario = lead.scenario, 
				replication = lead.replication,
				inc.mean = 0,
				inc.sd = 0,
				inc.risk = 0,
				inc.rate = 0)

lead.table = rbind(lead.table, DT)
lead.table = lead.table[order(scenario)]

rm(lead.scenario, lead.replication, DT)

# add bundle and supplier columns so we can see which bundles and suppliers face what increase

lead.table[, bundle := leadtimes$bundle]
lead.table[, supplier := leadtimes$supplier]

# lets create a table of all the runs we need to do

runs = data.table(expand.grid(replication = 1:reps, scenario = 2:max(dem.table$scenario)))
DT = data.table(replication = 1, scenario = 1)

runs = rbind(DT, runs)

rm(DT, UPPER, LOWER, reps)

}

# ----------------------------------------------------------------------------------- 
# ---- Run the Experiment -----------------------------------------------------------
# ----------------------------------------------------------------------------------- 

registerDoParallel(cores = 5)

results = foreach(it = 1:nrow(runs)) %dopar%
{

require(data.table)
require(foreach)

# ----------------------------------------------------------------------------------- 
# ---- Setting up a Run -------------------------------------------------------------
# ----------------------------------------------------------------------------------- 

# extract scenario and replication ids

it.s = runs$scenario[it]
it.r = runs$replication[it]

# extract the demand increase and leadtime increase for this iteration's scenario and replication

dem.inc = data.table(dem.table[scenario == it.s & replication == it.r])
lead.inc = data.table(lead.table[scenario == it.s & replication == it.r])

# create a copy of demands and leadtimes to then update with dem.inc and lead.inc

dem.dat = data.table(demands)
lead.dat = data.table(leadtimes)

# increase the mean and sd of product demands according to dem.inc

dem.dat[, norm.mean := norm.mean * (1 + dem.inc$inc.mean)]
dem.dat[, norm.sd := norm.sd * (1 + dem.inc$inc.sd)]

# increase the mean, sd, risk, and rate of supplier leadtimes according to lead.inc

lead.dat[, mean.norm := mean.norm * (1 + lead.inc$inc.mean)]
lead.dat[, sd.norm := sd.norm * (1 + lead.inc$inc.sd)]
lead.dat[, risk := risk * (1 + lead.inc$inc.risk)]

lead.dat[, rate.exp := rate.exp * (1 + ((-lead.inc$inc.rate) / (1 + lead.inc$inc.rate)))]

# ----------------------------------------------------------------------------------- 
# ---- Computing Bundle Demands -----------------------------------------------------
# ----------------------------------------------------------------------------------- 

# sample product demands

samp.demands = lapply(1:nrow(dem.dat), function(i) rnorm(samp, mean = dem.dat$norm.mean[i], sd = dem.dat$norm.sd[i]))
names(samp.demands) = dem.dat$product

# compute how many bundles each product is in

num.bundles = data.table(product = dem.dat$product, 
							num = sapply(dem.dat$product, function(i) nrow(bundles[product == i])))

# adjust the sampled demands in samp.demand by dividing each products demand by the number of bundles that product is in
# this adjustment is for aggregating product dem.dat into bundle dem.dat
	# where multiple bundles can be used to satisfy a products demand
	# this division means that we assume each bundle is used equally to satisfy a products demand

samp.demands = lapply(1:length(samp.demands), function(i) samp.demands[[i]] / num.bundles$num[i])
names(samp.demands) = dem.dat$product

rm(num.bundles)

# linear pooling of product dem.dat to create a bundles demand

bundle.demands = foreach(i = unique(bundles$bundle)) %do%
{
	# lets compute which product demands should be applied to bundle i
	
	apply.demand = as.numeric(sapply(names(samp.demands), function(j) any(bundles[bundle == i, product] %in% j)))
	
	# weights for each product demand, based on the mean demand
		# this is computed such that all weights sum up to one (ie. convex weights)
		# this is computed such that a higher demanding product with have a higher influence on the bundles demand
		# if a product isn't in bundle i then it'll have a weight of 0
		
	weight.demands = sapply(1:length(samp.demands), function(j) apply.demand[j] * mean(samp.demands[[j]]))
	weight.demands = weight.demands / sum(weight.demands)
	
	# multiply all demands in samp.demands by the weight.demands
	
	output = lapply(1:length(samp.demands), function(j) weight.demands[j] * samp.demands[[j]])
	
	# sum up the demands element wise to have linearly pooled demand for bundle i
	
	output = Reduce("+", output)
	
	# compute the mean and std. dev. of bundle i's pooled demand
	
	output = data.table(bundle = i, means = mean(output), sds = sd(output))
	return(output)
}

rm(i, apply.demand, output, weight.demands, samp.demands)

bundle.demands = rbindlist(bundle.demands)

# ----------------------------------------------------------------------------------- 
# ---- Computing Bundle Lead Times --------------------------------------------------
# ----------------------------------------------------------------------------------- 

# sample bundle leadtimes

bundle.leadtimes = lapply(1:nrow(lead.dat), function(i) rnorm(samp, mean = lead.dat$mean.norm[i], sd = lead.dat$sd.norm[i]))

# sample interuption delays

bundle.delays = lapply(1:nrow(lead.dat), function(i) rexp(samp, rate = lead.dat$rate.exp[i]) * sample(x = c(0, 1), size = samp, replace = TRUE, prob = c(1 - lead.dat$risk[i], lead.dat$risk[i])))

# add the delays to the leadtimes

bundle.leadtimes = lapply(1:nrow(lead.dat), function(i) bundle.leadtimes[[i]] + bundle.delays[[i]])

# compute the mean and std dev of the leadtimes

bundle.leadtimes = lapply(1:nrow(lead.dat), function(i) data.table(bundle = i, means = mean(bundle.leadtimes[[i]]), sds = sd(bundle.leadtimes[[i]])))
bundle.leadtimes = rbindlist(bundle.leadtimes)

rm(bundle.delays)

# ----------------------------------------------------------------------------------- 
# ---- Computing Bundle Safety Stocks -----------------------------------------------
# ----------------------------------------------------------------------------------- 

bundle.SS = foreach(i = 1:nrow(lead.dat)) %do%
{
	k = k.start
	
	# mean leadtime
	m_l = bundle.leadtimes$means[i]
	
	# leadtime std. dev.
	s_l = bundle.leadtimes$sds[i]
	
	# mean demand
	m_d = bundle.demands$means[i]
	
	# demand std. dev.
	s_d = bundle.demands$sds[i]
	
	# mean of demand during leadtime
	m_DDLT = m_l * m_d
	
	# std. dev. of demand during leadtime
	s_DDLT = sqrt((m_l * (s_d^2)) + ((m_d^2) * (s_l^2))) 
	
	# expected lost demand during leadtime
		# k is the safety stock weight we want to solve for
	L_k = (1/2) * (sqrt(1 + (k^2)) - k) * s_DDLT
	
	# solve for k in (F_k = 0) to meet the service level
	F_k = 1 - (((1/2) * (sqrt(1 + (k^2)) - k) * s_DDLT) / m_DDLT) - a
	
	# derivative of F_k with respect to k
		# see: https://www.wolframalpha.com/input/?i=d%2Fdk(1+-+(((1%2F2)+*+(sqrt(1+%2B+(k%5E2))+-+k)+*+sqrt((m_l+*+(s_d%5E2))+%2B+((m_d%5E2)+*+(s_l%5E2))))+%2F+(m_l+*+m_d))+-+a)
	dF_k = (-1/2) * ((((k / sqrt(1 + (k^2))) - 1) * s_DDLT) / m_DDLT)
	
	# use newtons method to iteratively find the root (ie. optimal k) of F_k
	
	# k.values will record values of k
	k.values = c(k)
	
	# done is our while loop parameter
	done = FALSE
	
	# apply newtons method
	
	while(done == FALSE)
	{
		F_k = 1 - (((1/2) * (sqrt(1 + (k^2)) - k) * s_DDLT) / m_DDLT) - a
		dF_k = (-1/2) * ((((k / sqrt(1 + (k^2))) - 1) * s_DDLT) / m_DDLT)
		
		k = k - (F_k / dF_k)
		k.values = c(k.values, k)
		
		done = identical(round(F_k, digits), 0)
	}
	
	rm(k.values, dF_k, done, m_l, s_l, m_d, s_d, L_k)
	
	# compute the service level
	
	service.level = F_k + a
	
	# compute the reorder point
	
	R = m_DDLT + (k * s_DDLT)
	
	# compute the saftey stock
	
	SS = R - m_DDLT
	
	rm(F_k, s_DDLT, m_DDLT)
	
	# build a table of bundle i's safety stock policy
	
	output = data.table(bundle = i, 
						service.level = service.level,
						R = R,
						SS = SS, 
						k = k)
	
	rm(service.level, R, SS, k)
	
	return(output)
}

rm(output, i)

bundle.SS = rbindlist(bundle.SS)
bundle.SS[, scenario := it.s]
bundle.SS[, replication := it.r]

return(bundle.SS)

}

registerDoSEQ()

results = rbindlist(results)

rm(runs, samp, digits, k.start, a)

# ----------------------------------------------------------------------------------- 
# ---- Building Descriptive Columns -------------------------------------------------
# ----------------------------------------------------------------------------------- 

{

# lets give each scenario a name so its not only a number

results[, scenario.name := ifelse(scenario == 1, "Baseline",

		ifelse(scenario == 2, "Increase Demand Mean of Product A",
		ifelse(scenario == 3, "Increase Demand Mean of Product B",
		ifelse(scenario == 4, "Increase Demand Mean of Product C",
		ifelse(scenario == 5, "Increase Demand Mean of All Products",
		
		ifelse(scenario == 6, "Increase Demand Std. Dev. of Product A",
		ifelse(scenario == 7, "Increase Demand Std. Dev. of Product B",
		ifelse(scenario == 8, "Increase Demand Std. Dev. of Product C",
		ifelse(scenario == 9, "Increase Demand Std. Dev. of All Products",

		ifelse(scenario == 10, "Increase Lead Time Mean of Supplier 1",
		ifelse(scenario == 11, "Increase Lead Time Mean of Supplier 2",
		ifelse(scenario == 12, "Increase Lead Time Mean of Supplier 3",
		ifelse(scenario == 13, "Increase Lead Time Mean of All Suppliers",
		
		ifelse(scenario == 14, "Increase Lead Time Std. Dev. of Supplier 1",
		ifelse(scenario == 15, "Increase Lead Time Std. Dev. of Supplier 2",
		ifelse(scenario == 16, "Increase Lead Time Std. Dev. of Supplier 3",
		ifelse(scenario == 17, "Increase Lead Time Std. Dev. of All Suppliers",

		ifelse(scenario == 18, "Increase Supply Risk of Supplier 1",
		ifelse(scenario == 19, "Increase Supply Risk of Supplier 2",
		ifelse(scenario == 20, "Increase Supply Risk of Supplier 3",
		ifelse(scenario == 21, "Increase Supply Risk of All Suppliers",
		
		ifelse(scenario == 22, "Increase Interruption Length of Supplier 1",
		ifelse(scenario == 23, "Increase Interruption Length of Supplier 2",
		ifelse(scenario == 24, "Increase Interruption Length of Supplier 3",
								"Increase Interruption Length of All Suppliers"))))))))))))))))))))))))]

results[, scenario.name := factor(scenario.name, levels = unique(scenario.name))]

# lets give each bundle a name so its not just a number

results[, bundle.name := ifelse(bundle == 1, "A",
		ifelse(bundle == 2, "B",
		ifelse(bundle == 3, "C",
		ifelse(bundle == 4, "AB",
		ifelse(bundle == 5, "AC",
		ifelse(bundle == 6, "BC",
							"ABC"))))))]

results[, bundle.name := factor(bundle.name, levels = unique(bundle.name))]

# lets create a column of the suppliers

results[, supplier := rep(c(1, 1, 1, 2, 2, 2, 3), nrow(results) / max(results$bundle))]

# lets build a column of baseline values for our three safety stock metrics: 
	# R (Reorder Point)
	# SS (Safety Stock)
	# k (Safety Stock Weight)

results[, R.base := rep(results[scenario == 1, R], nrow(results) / max(results$bundle))]
results[, SS.base := rep(results[scenario == 1, SS], nrow(results) / max(results$bundle))]
results[, k.base := rep(results[scenario == 1, k], nrow(results) / max(results$bundle))]

# lets compute the percent change in our three safety stock metrics across all scenarios

results[, R.change := (R - R.base) / R.base]
results[, SS.change := (SS - SS.base) / SS.base]
results[, k.change := (k - k.base) / k.base]

}

# ----------------------------------------------------------------------------------- 
# ---- Plot Safety Stock by Increasing Product Demand Mean --------------------------
# ----------------------------------------------------------------------------------- 

{

# lets define 'Times' to represent times new roman font for our plots

windowsFonts(Times = windowsFont("TT Times New Roman"))

# lets extract all scenarios regarding: Increasing Product Demand Mean

DT = data.table(results[scenario %in% 2:5])

# ---- plot change in reorder point -------------------------------------------------

R.inc.dem.mean.plot = ggplot(data = DT, aes(x = bundle.name, y = R.change, fill = bundle.name)) +
						geom_boxplot() +
						geom_hline(yintercept = 0, color = "red", size = 1, alpha = 1/3) +
						scale_y_continuous(labels = percent) +
						facet_wrap(~scenario.name) +
						labs(x = "Bundles", y = "Change") +
						ggtitle("Change in Reorder Point") +
						theme_bw(base_size = 30, base_family = "Times") +
						theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

R.inc.dem.mean.plot

# ---- plot change in safety stock --------------------------------------------------

SS.inc.dem.mean.plot = ggplot(data = DT, aes(x = bundle.name, y = SS.change, fill = bundle.name)) +
						geom_boxplot() +
						geom_hline(yintercept = 0, color = "red", size = 1, alpha = 1/3) +
						scale_y_continuous(labels = percent) +
						facet_wrap(~scenario.name) +
						labs(x = "Bundles", y = "Change") +
						ggtitle("Change in Safety Stock") +
						theme_bw(base_size = 30, base_family = "Times") +
						theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

SS.inc.dem.mean.plot

# ---- plot change in safety stock weight -------------------------------------------

k.inc.dem.mean.plot = ggplot(data = DT, aes(x = bundle.name, y = k.change, fill = bundle.name)) +
						geom_boxplot() +
						geom_hline(yintercept = 0, color = "red", size = 1, alpha = 1/3) +
						scale_y_continuous(labels = percent) +
						facet_wrap(~scenario.name) +
						labs(x = "Bundles", y = "Change") +
						ggtitle("Change in Safety Stock Weight") +
						theme_bw(base_size = 30, base_family = "Times") +
						theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

k.inc.dem.mean.plot

}

# ----------------------------------------------------------------------------------- 
# ---- Plot Safety Stock by Increasing Product Demand Std. Dev. ---------------------
# ----------------------------------------------------------------------------------- 

{

# lets extract all scenarios regarding: Increasing Product Demand Std. Dev.

DT = data.table(results[scenario %in% 6:9])

# ---- plot change in reorder point -------------------------------------------------

R.inc.dem.sd.plot = ggplot(data = DT, aes(x = bundle.name, y = R.change, fill = bundle.name)) +
						geom_boxplot() +
						geom_hline(yintercept = 0, color = "red", size = 1, alpha = 1/3) +
						scale_y_continuous(labels = percent) +
						facet_wrap(~scenario.name) +
						labs(x = "Bundles", y = "Change") +
						ggtitle("Change in Reorder Point") +
						theme_bw(base_size = 30, base_family = "Times") +
						theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

R.inc.dem.sd.plot

# ---- plot change in safety stock --------------------------------------------------

SS.inc.dem.sd.plot = ggplot(data = DT, aes(x = bundle.name, y = SS.change, fill = bundle.name)) +
						geom_boxplot() +
						geom_hline(yintercept = 0, color = "red", size = 1, alpha = 1/3) +
						scale_y_continuous(labels = percent) +
						facet_wrap(~scenario.name) +
						labs(x = "Bundles", y = "Change") +
						ggtitle("Change in Safety Stock") +
						theme_bw(base_size = 30, base_family = "Times") +
						theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

SS.inc.dem.sd.plot

# ---- plot change in safety stock weight -------------------------------------------

k.inc.dem.sd.plot = ggplot(data = DT, aes(x = bundle.name, y = k.change, fill = bundle.name)) +
						geom_boxplot() +
						geom_hline(yintercept = 0, color = "red", size = 1, alpha = 1/3) +
						scale_y_continuous(labels = percent) +
						facet_wrap(~scenario.name) +
						labs(x = "Bundles", y = "Change") +
						ggtitle("Change in Safety Stock Weight") +
						theme_bw(base_size = 30, base_family = "Times") +
						theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

k.inc.dem.sd.plot

}

# ----------------------------------------------------------------------------------- 
# ---- Plot Safety Stock by Increasing Supplier Lead Time Mean ----------------------
# ----------------------------------------------------------------------------------- 

{

# lets extract all scenarios regarding: Increasing Supplier Lead Time Mean

DT = data.table(results[scenario %in% 10:13])

# ---- plot change in reorder point -------------------------------------------------

R.inc.lead.mean.plot = ggplot(data = DT, aes(x = bundle.name, y = R.change, fill = factor(supplier))) +
						geom_boxplot() +
						geom_hline(yintercept = 0, color = "red", size = 1, alpha = 1/3) +
						scale_y_continuous(labels = percent) +
						facet_wrap(~scenario.name) +
						labs(x = "Bundles", y = "Change", fill = "Supplier") +
						ggtitle("Change in Reorder Point") +
						theme_bw(base_size = 30, base_family = "Times") +
						theme(plot.title = element_text(hjust = 0.5), legend.position = "top") +
						guides(fill = guide_legend(nrow = 1, override.aes = list(size = 10, linetype = 0, alpha = 1)))

R.inc.lead.mean.plot

# ---- plot change in safety stock --------------------------------------------------

SS.inc.lead.mean.plot = ggplot(data = DT, aes(x = bundle.name, y = SS.change, fill = factor(supplier))) +
						geom_boxplot() +
						geom_hline(yintercept = 0, color = "red", size = 1, alpha = 1/3) +
						scale_y_continuous(labels = percent) +
						facet_wrap(~scenario.name) +
						labs(x = "Bundles", y = "Change", fill = "Supplier") +
						ggtitle("Change in Safety Stock") +
						theme_bw(base_size = 30, base_family = "Times") +
						theme(plot.title = element_text(hjust = 0.5), legend.position = "top") +
						guides(fill = guide_legend(nrow = 1, override.aes = list(size = 10, linetype = 0, alpha = 1)))

SS.inc.lead.mean.plot

# ---- plot change in safety stock weight -------------------------------------------

k.inc.lead.mean.plot = ggplot(data = DT, aes(x = bundle.name, y = k.change, fill = factor(supplier))) +
						geom_boxplot() +
						geom_hline(yintercept = 0, color = "red", size = 1, alpha = 1/3) +
						scale_y_continuous(labels = percent) +
						facet_wrap(~scenario.name) +
						labs(x = "Bundles", y = "Change", fill = "Supplier") +
						ggtitle("Change in Safety Stock Weight") +
						theme_bw(base_size = 30, base_family = "Times") +
						theme(plot.title = element_text(hjust = 0.5), legend.position = "top") +
						guides(fill = guide_legend(nrow = 1, override.aes = list(size = 10, linetype = 0, alpha = 1)))

k.inc.lead.mean.plot

}

# ----------------------------------------------------------------------------------- 
# ---- Plot Safety Stock by Increasing Supplier Lead Time Std. Dev. -----------------
# ----------------------------------------------------------------------------------- 

{

# lets extract all scenarios regarding: Increasing Supplier Lead Time Std. Dev.

DT = data.table(results[scenario %in% 14:17])

# ---- plot change in reorder point -------------------------------------------------

R.inc.lead.sd.plot = ggplot(data = DT, aes(x = bundle.name, y = R.change, fill = factor(supplier))) +
						geom_boxplot() +
						geom_hline(yintercept = 0, color = "red", size = 1, alpha = 1/3) +
						scale_y_continuous(labels = percent) +
						facet_wrap(~scenario.name) +
						labs(x = "Bundles", y = "Change", fill = "Supplier") +
						ggtitle("Change in Reorder Point") +
						theme_bw(base_size = 30, base_family = "Times") +
						theme(plot.title = element_text(hjust = 0.5), legend.position = "top") +
						guides(fill = guide_legend(nrow = 1, override.aes = list(size = 10, linetype = 0, alpha = 1)))

R.inc.lead.sd.plot

# ---- plot change in safety stock --------------------------------------------------

SS.inc.lead.sd.plot = ggplot(data = DT, aes(x = bundle.name, y = SS.change, fill = factor(supplier))) +
						geom_boxplot() +
						geom_hline(yintercept = 0, color = "red", size = 1, alpha = 1/3) +
						scale_y_continuous(labels = percent) +
						facet_wrap(~scenario.name) +
						labs(x = "Bundles", y = "Change", fill = "Supplier") +
						ggtitle("Change in Safety Stock") +
						theme_bw(base_size = 30, base_family = "Times") +
						theme(plot.title = element_text(hjust = 0.5), legend.position = "top") +
						guides(fill = guide_legend(nrow = 1, override.aes = list(size = 10, linetype = 0, alpha = 1)))

SS.inc.lead.sd.plot

# ---- plot change in safety stock weight -------------------------------------------

k.inc.lead.sd.plot = ggplot(data = DT, aes(x = bundle.name, y = k.change, fill = factor(supplier))) +
						geom_boxplot() +
						geom_hline(yintercept = 0, color = "red", size = 1, alpha = 1/3) +
						scale_y_continuous(labels = percent) +
						facet_wrap(~scenario.name) +
						labs(x = "Bundles", y = "Change", fill = "Supplier") +
						ggtitle("Change in Safety Stock Weight") +
						theme_bw(base_size = 30, base_family = "Times") +
						theme(plot.title = element_text(hjust = 0.5), legend.position = "top") +
						guides(fill = guide_legend(nrow = 1, override.aes = list(size = 10, linetype = 0, alpha = 1)))

k.inc.lead.sd.plot

}

# ----------------------------------------------------------------------------------- 
# ---- Plot Safety Stock by Increasing Supplier Supply Risk -------------------------
# ----------------------------------------------------------------------------------- 

{

# lets extract all scenarios regarding: Increasing Supplier Supply Risk

DT = data.table(results[scenario %in% 18:21])

# ---- plot change in reorder point -------------------------------------------------

R.inc.lead.risk.plot = ggplot(data = DT, aes(x = bundle.name, y = R.change, fill = factor(supplier))) +
						geom_boxplot() +
						geom_hline(yintercept = 0, color = "red", size = 1, alpha = 1/3) +
						scale_y_continuous(labels = percent) +
						facet_wrap(~scenario.name) +
						labs(x = "Bundles", y = "Change", fill = "Supplier") +
						ggtitle("Change in Reorder Point") +
						theme_bw(base_size = 30, base_family = "Times") +
						theme(plot.title = element_text(hjust = 0.5), legend.position = "top") +
						guides(fill = guide_legend(nrow = 1, override.aes = list(size = 10, linetype = 0, alpha = 1)))

R.inc.lead.risk.plot

# ---- plot change in safety stock --------------------------------------------------

SS.inc.lead.risk.plot = ggplot(data = DT, aes(x = bundle.name, y = SS.change, fill = factor(supplier))) +
						geom_boxplot() +
						geom_hline(yintercept = 0, color = "red", size = 1, alpha = 1/3) +
						scale_y_continuous(labels = percent) +
						facet_wrap(~scenario.name) +
						labs(x = "Bundles", y = "Change", fill = "Supplier") +
						ggtitle("Change in Safety Stock") +
						theme_bw(base_size = 30, base_family = "Times") +
						theme(plot.title = element_text(hjust = 0.5), legend.position = "top") +
						guides(fill = guide_legend(nrow = 1, override.aes = list(size = 10, linetype = 0, alpha = 1)))

SS.inc.lead.risk.plot

# ---- plot change in safety stock weight -------------------------------------------

k.inc.lead.risk.plot = ggplot(data = DT, aes(x = bundle.name, y = k.change, fill = factor(supplier))) +
						geom_boxplot() +
						geom_hline(yintercept = 0, color = "red", size = 1, alpha = 1/3) +
						scale_y_continuous(labels = percent) +
						facet_wrap(~scenario.name) +
						labs(x = "Bundles", y = "Change", fill = "Supplier") +
						ggtitle("Change in Safety Stock Weight") +
						theme_bw(base_size = 30, base_family = "Times") +
						theme(plot.title = element_text(hjust = 0.5), legend.position = "top") +
						guides(fill = guide_legend(nrow = 1, override.aes = list(size = 10, linetype = 0, alpha = 1)))

k.inc.lead.risk.plot

}

# ----------------------------------------------------------------------------------- 
# ---- Plot Safety Stock by Increasing Supplier Interruption Length -----------------
# ----------------------------------------------------------------------------------- 

{

# lets extract all scenarios regarding: Increasing Supplier Interruption Length

DT = data.table(results[scenario %in% 22:25])

# ---- plot change in reorder point -------------------------------------------------

R.inc.lead.rate.plot = ggplot(data = DT, aes(x = bundle.name, y = R.change, fill = factor(supplier))) +
						geom_boxplot() +
						geom_hline(yintercept = 0, color = "red", size = 1, alpha = 1/3) +
						scale_y_continuous(labels = percent) +
						facet_wrap(~scenario.name) +
						labs(x = "Bundles", y = "Change", fill = "Supplier") +
						ggtitle("Change in Reorder Point") +
						theme_bw(base_size = 30, base_family = "Times") +
						theme(plot.title = element_text(hjust = 0.5), legend.position = "top") +
						guides(fill = guide_legend(nrow = 1, override.aes = list(size = 10, linetype = 0, alpha = 1)))

R.inc.lead.rate.plot

# ---- plot change in safety stock --------------------------------------------------

SS.inc.lead.rate.plot = ggplot(data = DT, aes(x = bundle.name, y = SS.change, fill = factor(supplier))) +
						geom_boxplot() +
						geom_hline(yintercept = 0, color = "red", size = 1, alpha = 1/3) +
						scale_y_continuous(labels = percent) +
						facet_wrap(~scenario.name) +
						labs(x = "Bundles", y = "Change", fill = "Supplier") +
						ggtitle("Change in Safety Stock") +
						theme_bw(base_size = 30, base_family = "Times") +
						theme(plot.title = element_text(hjust = 0.5), legend.position = "top") +
						guides(fill = guide_legend(nrow = 1, override.aes = list(size = 10, linetype = 0, alpha = 1)))

SS.inc.lead.rate.plot

# ---- plot change in safety stock weight -------------------------------------------

k.inc.lead.rate.plot = ggplot(data = DT, aes(x = bundle.name, y = k.change, fill = factor(supplier))) +
						geom_boxplot() +
						geom_hline(yintercept = 0, color = "red", size = 1, alpha = 1/3) +
						scale_y_continuous(labels = percent) +
						facet_wrap(~scenario.name) +
						labs(x = "Bundles", y = "Change", fill = "Supplier") +
						ggtitle("Change in Safety Stock Weight") +
						theme_bw(base_size = 30, base_family = "Times") +
						theme(plot.title = element_text(hjust = 0.5), legend.position = "top") +
						guides(fill = guide_legend(nrow = 1, override.aes = list(size = 10, linetype = 0, alpha = 1)))

k.inc.lead.rate.plot

}










