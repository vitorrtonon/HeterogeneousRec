library(foreach)
library(iterators)
library(doParallel)

cl = makeCluster(10)
clusterEvalQ(cl, .libPaths(c(.libPaths())))
registerDoParallel(cl)

increment.type <- function(type.i, type) {

	if(type.i == length(type)) 
		type.i = 1
	else
		type.i = type.i + 1

	return(type.i)
}

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

		#cat('Finished: ', j / length(users) * 100, '%\n')
	}

	return(all.walks)
}


# Generate Random Walks for all folds


foreach(fold.i=1:10) %dopar% {
#for(fold.i in 1:1) {

	library(reshape2)
	library(SnowballC)
	library(lsa)
	library(wordVectors)

	cat('Starting fold #', fold.i, '\n')

	directory = paste0("data/frappe/folds/fold", fold.i, "/") 

	data = read.csv(paste0(directory, "train.csv"))
	colnames(data) = c("user_id", "item_id", "context", "rating")
	data[,'user_id'] = paste0('aU_', data[,'user_id'])
	data[,'item_id'] = paste0('vI_', data[,'item_id'])
	data[,'context'] = paste0('iC_', data[,'context'])

	user.item = acast(data, user_id ~ item_id, value.var="rating", fun.aggregate=mean)
	user.item[is.nan(user.item)] = 0

	user.context = acast(data, user_id ~ context, value.var="rating", fun.aggregate=mean)
	user.context[is.nan(user.context)] = 0

	item.context = acast(data, item_id ~ context, value.var="rating", fun.aggregate=mean)
	item.context[is.nan(item.context)] = 0

   users = rownames(user.item)
 	items = colnames(user.item)

	type = c(1,2)
	adj.matrix.list = list(user.item, t(user.item))
	walks = generate.random.walks.from.metapaths(type, adj.matrix.list)
	lapply(walks, function(x) { write.table(t(data.frame(x)), file=paste0(directory, "walk_uiu.txt"), append=T, sep=" ", col.names=F, row.names=F, quote=F) } )
	
	type = c(1,2,3,4)
	adj.matrix.list = list(user.item, item.context, t(item.context), t(user.item))
	walks = generate.random.walks.from.metapaths(type, adj.matrix.list)
	lapply(walks, function(x) { write.table(t(data.frame(x)), file=paste0(directory, "walk_uiciu.txt"), sep=" ", append=T, col.names=F, row.names=F, quote=F) } )

	system(paste0('code_metapath2vec/metapath2vec -train ', directory, 'walk_uiu.txt -output ', directory, 'train_uiu.emb -pp 1 -size 100 -min-count 0 -threads 1'))
	system(paste0('code_metapath2vec/metapath2vec -train ', directory, 'walk_uiciu.txt -output ', directory, 'train_uiciu.emb -pp 1 -size 100 -min-count 0 -threads 1'))

       
	topN = 10
	embeddings.uiu = read.binary.vectors(paste0(directory, "train_uiu.emb"))
	embeddings.uiciu = read.binary.vectors(paste0(directory, "train_uiciu.emb"))

	items.embeddings.ids = grep("vI_", rownames(embeddings.uiu))
	items.embeddings = embeddings.uiu[items.embeddings.ids,]
	all.recommendations.uiu = data.frame() 
	all.recommendations.uiciu = data.frame() 
	for(user in users) {

		user.embedding.uiu = embeddings.uiu[user,]
		user.embedding.uiciu = embeddings.uiciu[user,]

		item.similarities = apply(items.embeddings, 1, function(row) { cosine(as.numeric(user.embedding.uiu), as.numeric(row)) } )
		topN.items = names(sort(item.similarities, dec=T)[1:topN])
		all.recommendations.uiu = rbind(all.recommendations.uiu, t(data.frame(topN.items)))

		item.similarities = apply(items.embeddings, 1, function(row) { cosine(as.numeric(user.embedding.uiciu), as.numeric(row)) } )
		topN.items = names(sort(item.similarities, dec=T)[1:topN])
		all.recommendations.uiciu = rbind(all.recommendations.uiciu, t(data.frame(topN.items)))
	}

	rownames(all.recommendations.uiu) = users
	write.table(all.recommendations.uiu, file=paste0(directory, 'recommendations_uiu.csv'), sep=",", row.names=T, col.names=F, quote=F)

	rownames(all.recommendations.uiciu) = users
	write.table(all.recommendations.uiciu, file=paste0(directory, 'recommendations_uiciu.csv'), sep=",", row.names=T, col.names=F, quote=F)

}


