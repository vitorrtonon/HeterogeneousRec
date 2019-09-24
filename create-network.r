library(reshape2)

increment.type <- function(type.i, type) {

	if(type.i == length(type)) 
		type.i = 1
	else
		type.i = type.i + 1

	return(type.i)
}

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

generate.random.walks.from.metapaths <- function(type = c(1,2),
												adj.matrix.list=list(user.item, t(user.item)),
												walks.per.node = 1000,
												max.walk.length = 100) {
	count = 1
	all.walks = list()

	for (j in 1:length(users)) {

		user = users[j]

		for(n.walks in 1:walks.per.node) {

			walk.length = 1

			previous.selected.node = user
			walk = c(user)

			type.i = 1

			while(walk.length <= max.walk.length || type.i != 1) {

				nodes.ids = which(adj.matrix.list[[type.i]][previous.selected.node,] != 0)
			
				if(length(nodes.ids) > 0) 
					selected.node.id = sample(x=nodes.ids, size=1)
				else 
					selected.node.id = sample(x=length(colnames(adj.matrix.list[[type.i]])), size=1)
				
				selected.node = colnames(adj.matrix.list[[type.i]])[selected.node.id]

				type.i = increment.type(type.i, type)

				walk = c(walk, selected.node)
				previous.selected.node = selected.node

				walk.length = walk.length + 1
			}
		
			all.walks[[count]] = walk
			count = count + 1
		}

		cat('Finished: ', j / length(users) * 100, '%\n')
	}

	return(all.walks)
}

