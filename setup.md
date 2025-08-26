# 🧰 DS400 – Class Setup Instructions

**Instructor**: Connor Flynn  
**Course**: DS400  
**Topic**: GitHub + R Setup  

---

## ✅ Today’s Goals

By the end of class, you will have:

- Updated **R** to the latest version  
- Installed and tested the **`bayesrules`** package  
- Created a new **GitHub repository**  
- Linked it with a new **R Project** in RStudio  
- Verified your **GitHub credentials and tokens**  

---

## 🔧 Step 1: Update R

Make sure you have the latest version of R installed:  
[Download R here](https://cran.r-project.org/)

---

## 📦 Step 2: Install and Test `bayesrules`

Run these commands in your R console:

```r
install.packages("bayesrules")
library(bayesrules)
``` 


## 📦 Step 3: Create a  New Github Repository

- Click New Repository
- Name it DS400
- ✅ **Check** the box to add a `README.md`
- Click Create Repository
- > 📎 Your repo URL will look like: `https://github.com/yourusername/DS400`
  

##  🔗 Step 4: Link to an R Project in RStudio

- Open RStudio
- Go to File → New Project → Version Control → Git
- Paste the GitHub repository URL
- Choose a local folder for the project
- Click Create Project


## 🔐 Step 5: Verify GitHub Credentials & Token

Run the following in your R console:

```r
usethis::git_sitrep()
``` 

You should see something like this:

<img width="938" height="544" alt="image" src="https://github.com/user-attachments/assets/49612a89-f250-4656-8a40-6966e95586ad" />



If token is not **discovered**, you need to:


### Generate and Store a GitHub PAT
A PAT (Personal Access Token) is needed to push to GitHub securely.

1. In the Console tab (next to Terminal):

```r
usethis::create_github_token()
```
2. This opens a GitHub page. Name the token (e.g. `ds421-laptop`) and click **Generate token**.
3. Copy the token to your clipboard (you won't be able to see it again).

4. Back in the Console:
```r
gitcreds::gitcreds_set()
```
5. Paste your token when prompted.




