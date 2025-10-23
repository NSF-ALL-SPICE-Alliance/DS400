# 🌊 Bayesian Fishpond Modeling on Jetstream2  
### DS400: Bayesian Statistics — Scientific Gateways Hack Week Edition

This guide will walk you through:

1️⃣ Creating and logging into your **ACCESS** account  
2️⃣ Launching a cloud virtual machine on **Jetstream2**  
3️⃣ Uploading the fishpond dataset and modeling script  
4️⃣ Running a Bayesian model of dissolved oxygen in **RStudio**  
5️⃣ Understanding the environmental context and results

---

## ✅ Why are we doing this?

We’re working with a large, real-world environmental dataset collected every 10 minutes from a traditional Hawaiian fishpond. We want to understand how **temperature** and **pH** influence **dissolved oxygen (DO)**—especially during heat stress.

Warmer water:
- holds **less oxygen**
- reduces the benefits of **photosynthesis** (which raises pH)
- increases **hypoxia risk** for fish

We are also interested in how restoring **groundwater flow** (cooler water under the highway) may **improve oxygen resilience** over time.

This dataset is:
- large (hundreds of thousands of records)
- irregular across sensors
- autocorrelated in time

➡️ Using a **Bayesian hierarchical AR(1) model** is the scientifically correct approach  
➡️ It’s computationally heavy ⚠️  
➡️ **Jetstream2** provides the cloud computing power needed ✅

---

## 🚀 Step 1 — Create an ACCESS Account

1️⃣ Visit → https://access-ci.org  
2️⃣ Click **Sign Up**  
3️⃣ Choose **Researcher** for your role  
4️⃣ Verify your email and finish your profile  

This grants you access to NSF cyberinfrastructure systems.

---

## 🔑 Step 2 — You are added to our Jetstream2 allocation

Your instructor will add you to:

> **CIS250924: NSF SCIPE — Cyberinfrastructure Pacific Professionals**

This gives you compute resources.

✅ Watch for an email confirming you were added  
✅ No action needed from you

---

## 🖥️ Step 3 — Log into Jetstream2

1️⃣ Go to: https://jetstream2.exosphere.app/exosphere/  
2️⃣ Log in with your **ACCESS** credentials  
3️⃣ Click **Launch New Instance**

Recommended settings:
- **Project**: CIS250924
- **Image**: Ubuntu 22.04
- **Instance Size**: `m3.small` (2 vCPU / 8GB RAM)
- Name it: **fishpond-bayes**

Click **Create Instance**

---

## 💻 Step 4 — Open a Web Desktop

Once the VM finishes booting:

1️⃣ Select your instance  
2️⃣ Click **Open Web Desktop**  
3️⃣ You will see a Linux desktop in your browser  

✅ You are now using a cloud computer!

---

## 📁 Step 5 — Upload files to your VM

You will need two files:

| File | Link |
|------|------|
| `master_data_pivot.csv` | https://github.com/NSF-ALL-SPICE-Alliance/MFHC/blob/main/cleaned_data/master_data_pivot.csv |
| `fishpond_modeling.R` | https://github.com/NSF-ALL-SPICE-Alliance/DS400/ |

Download both files to your laptop, then **drag-and-drop** them into the **File Manager** on your Jetstream2 Web Desktop (e.g., into the *Documents* folder).

---

## 🧠 Step 6 — Run the model in RStudio

1️⃣ Open the **Application Menu** → launch **RStudio**  
2️⃣ Click **File > Open File** and open `fishpond_modeling.R`  
3️⃣ Install required packages (run the first few lines)  
4️⃣ Hold **Ctrl + Enter** (Windows/Linux) or **Cmd + Return** (Mac keyboard)  
   to execute code line-by-line

💡 Tip: Run each block and make sure output looks good before continuing.

The model will take some time to run — this is where Jetstream2’s CPUs help! ⚡

---

## ✅ What the model tells us

This Bayesian model estimates how DO changes with:
- **temperature** (heat stress)
- **pH** (photosynthesis signal)
- **time of day**
- **season**
- **differences between sites**

### Key findings (in plain language)

- During the day when plants produce oxygen, **pH rises** and DO usually rises too  
- But **warm water weakens** that oxygen boost  
- This means hot days = **higher risk of low oxygen**  
- If the planned restoration cools the pond by ~**2°C**, DO could improve by **~0.3–0.6 mg/L** during productive periods  
- This model gives us a **baseline** to measure success **after** groundwater is restored

> In short: **cooler water means healthier ponds and fewer low-oxygen events**

---

## 🌐 Why Jetstream2 matters here

- We keep **all** the data — not just the cleanest bits  
- The model uses **complex correlations** over time and space  
- This requires **more RAM and CPU power** than a typical laptop  
- Students gain **hands-on access** to real cyberinfrastructure used in national research

> Jetstream2 turns a challenging, real dataset into a practical learning experience.

---

If you need help:
📧 connor.flynn@chaminade.edu  


Happy cloud modeling! ☁️🐟📈
