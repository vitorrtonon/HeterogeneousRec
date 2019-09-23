library(reshape2)

fold.i = 1
directory = paste0("data/frappe/folds/fold", fold.i, "/") 

data = read.csv(paste0(directory, "train.csv"))
colnames(data) = c("user_id", "item_id", "context", "rating")
data[,'user_id'] = paste0('U_', data[,'user_id'])
data[,'item_id'] = paste0('I_', data[,'item_id'])
data[,'context'] = paste0('C_', data[,'context'])

user.item = acast(data, user_id ~ item_id, value.var="rating", fun.aggregate=mean)
user.item[is.nan(user.item)] = 0

user.context = acast(data, user_id ~ context, value.var="rating", fun.aggregate=mean)
user.context[is.nan(user.context)] = 0

item.context = acast(data, item_id ~ context, value.var="rating", fun.aggregate=mean)
item.context[is.nan(item.context)] = 0


users = rownames(user.item)

##################################################################################3

# METAPATH UIU

# for each node U
# 	for count in 1:walks.per.node
# 		for l in 1:walk.length
# 			pop type t from metapath
# 			select only the nodes of type t+1 that node of type t has a connection to
#			choose one according to probability 1/length
#			push type t into metapath

type.i = 1
for (user in users) {

	for(count in 1:walks.per.node) {

		while(wl < walk.length && type[current.node] != "user") {


		}
	}
}
