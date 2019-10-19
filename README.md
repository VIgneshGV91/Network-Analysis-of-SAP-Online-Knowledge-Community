# Network-Analysis-of-SAP-Online-Knowledge-Community
This project is the analysis of the implications of Network Structure of the SAP Online Knowledge Community Platform
Implications of Network Structure of the SAP Online Knowledge Community Platform

The objective of this project is to examine a network drawn from the SAP Online Knowledge Community platform (http://go.sap.com/community.html). This is an induced sub-network based on 10% of nodes drawn from the dataset generously provided by Prof. Peng Huang, of the University of Maryland. See here for more on his research studies: http://www.rhsmith.umd.edu/directory/peng-­‐ huang.

The provided dataset CSV file is a directed edge list. The right‐hand column represents the ID of a user who posts a question that starts a new thread in a SAP community user forum. The left‐hand column represents the ID of a user who provides an answer to the posted question. So, a directed edge from the left‐hand node to a right‐hand node represents an answer provided to a question. Since a user can answer multiple questions posted by another user on one or more threads, the file contains duplicate edge‐pairs, which can be combined to form directed edges of varying weights (using the simplify function in R).

The project includes analysis and findings of the features of the network, its overall structure, the distributions of network measures among nodes and edges, and all their implications of SAP knowledge community. The findings also include trust, reciprocity, authority, and access to information,strong triadic closure is present in this network, and what the data tells us about the role of local bridges and weak ties. 
