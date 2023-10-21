using PkgTemplates

t = Template(;
    user="matthewghgriffiths",
    authors=["Matthew Griffiths"],
    plugins=[
        License(name="MIT"),
        Git(),
        GitHubActions(),
    ],
)

t("BTree.jl")