# Code to plot reconstruction of  inferred rate categories from MiSSE
# Orlando Schwery 9. July 2023

# rates should be the object that gets returned from running MarginReconMiSSE
# nrates is the number of rate categories (I gotta automate that sometime)
# type is the plot type ('fan', 'phylogram', etc.)

stateplot <- function(rates, nrates, cols=NULL, type, tiplab=FALSE, cex=0.5) {
    plot(rates$phy, type=type, show.tip.label=tiplab, label.offset=1, cex=cex)
    if (is.null(cols)) {
        col <- c("darkcyan", "gold", "maroon2", "darkorchid", "seagreen2")  # the colours need to be in the same order as the trait states
    } else {
        col <- cols
    }
    # tiplabels(pch=22, bg=col[as.numeric(as.factor(discretes[,2]))], cex=2, adj=1)  # we get the tip state coloured based on what the state is in the original data (with some subsetting etc)
    tiplabels(pie = rates$tip.mat[, 2:(nrates+1)], piecol = col, cex = 0.25, adj=1)  # we get the node states coloured based on the reconstruction $lik.anc is the likelihood values for each of the different trait states at 
    nodelabels(pie = rates$node.mat[, 2:(nrates+1)], piecol = col, cex = 0.25)  # we get the node states coloured based on the reconstruction $lik.anc is the likelihood values for each of the different trait states at 
    legend("bottomleft", title="Hidden States", legend=colnames(rates$tip.mat)[2:(nrates+1)], fill=col, horiz=FALSE)

}


# Usage:

stateplot(four.rate.recon, 4, "phylogram")
