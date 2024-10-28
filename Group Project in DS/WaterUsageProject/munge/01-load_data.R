# Load Data ---------------------------------------------------------------

agg = read.table("data/aggregatedWholeHouse.csv", sep = ',', header = 1) # sep - csv, header - colnames
bidet = read.table("data/feedBidet.csv", sep = ',', header = 1)
dishwasher = read.table("data/feedDishwasher.csv", sep = ',', header = 1)
kitchen_faucet = read.table("data/feedKitchenfaucet.csv", sep = ',', header = 1)
shower = read.table("data/feedShower.csv", sep = ',', header = 1)
sink = read.table("data/feedWashbasin.csv", sep = ',', header = 1)
toilet = read.table("data/feedToilet.csv", sep = ',', header = 1)
washing = read.table("data/feedWashingmachine.csv", sep = ',', header = 1)

# Raw Data ----------------------------------------------------------------

raw_agg = agg
raw_bidet = bidet
raw_dishwasher = dishwasher
raw_faucet = kitchen_faucet
raw_shower = shower
raw_sink = sink
raw_toilet = toilet
raw_washing = washing
