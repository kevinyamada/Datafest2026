# Keeping Type 2 Diabetes Patients Connected

## Overview

This project was created for ASA DataFest 2026 at the University of Toronto. Our team, Fourcast, placed third as finalists.

The project focuses on follow up gaps among Type 2 diabetes patients. Type 2 diabetes requires ongoing care, but patients may go long periods without another diabetes related encounter. Our goal was to identify patients who are more likely to experience delayed follow up so that healthcare teams can prioritize support earlier.

## Problem

Healthcare systems collect millions of encounter records, but individual encounters do not always show whether patients remain connected to care over time.

For Type 2 diabetes, delayed follow up can mean missed opportunities for monitoring, lab work, medication review, and care planning.

Our main question was:

> Can we identify Type 2 diabetes patients who are more likely to go over 180 days without another diabetes-related encounter?

## Data

The analysis used de-identified healthcare encounter data provided during DataFest. The raw data is not included in this repository because it contains private healthcare information and was only available for competition use.

Main files used in the analysis included:

- encounters
- patients
- diagnosis
- departments
- social determinants

## Methodology

We focused only on Type 2 diabetes because follow up expectations vary widely across different diagnoses. A long gap may be normal for one condition but more concerning for a chronic condition like diabetes.

A Type 2 diabetes journey was defined as all diabetes-related encounters for the same patient.

For each journey, we:

1. Sorted diabetes-related encounters by date.
2. Calculated the number of days between consecutive encounters.
3. Found the maximum gap for each patient.
4. Flagged whether the patient had any gap longer than 180 days.

To reduce bias from the fixed data window, we only included patients with at least 365 days of observation time. We also removed first encounter year from the final model because it mostly reflected the data window rather than real patient risk.

## Model

We used a random forest model to predict whether a patient would experience a follow-up gap longer than 180 days.

The model used early patient and encounter information, including:

- first department specialty
- first encounter type
- first visit type description
- age group
- MyChart status
- social determinant screening information
- recorded social risk indicators

The model grouped patients into three follow up risk tiers:

- Low risk
- Medium risk
- High risk

## Key Results

After filtering to patients with at least 365 days of observation time:

- 20,409 Type 2 diabetes journeys were included.
- 74.8% had at least one follow-up gap longer than 180 days.
- The median maximum gap was 258 days.

The model separated patients into meaningful risk groups:

| Risk Tier | Actual 180-Day Gap Rate |
|---|---:|
| Low | 61.9% |
| Medium | 79.9% |
| High | 85.0% |

The most important predictors were mostly early care-context variables, especially first department specialty, SDOH domains answered, first visit type description, first encounter type, and recorded social risk.

## Recommendation

We proposed a Type 2 Diabetes Follow-Up Risk Protocol.

Instead of treating all diabetes patients the same, the healthcare system could use risk tiers to guide follow-up intensity:

| Risk Tier | Suggested Follow-Up Action |
|---|---|
| Low | Standard follow up process |
| Medium | Reminder before the 6 month mark |
| High | Schedule next diabetes follow-up before the patient leaves, plus earlier reminders or check ins |

This approach does not prove what causes follow up gaps. Instead, it helps identify patients who may need more follow up support.

## Limitations

- This project is predictive, not causal.
- Some follow up gaps may be clinically appropriate.
- Social determinant variables depend on whether patients were screened.
- MyChart and outreach variables should be interpreted as engagement or workflow signals, not proof that those tools cause better follow up.
- Raw data cannot be shared publicly as per competitiion rules.

## Repository Structure

```text
scripts/      R scripts used for data preparation, modeling, and visualization
figures/      Final visualizations used in the presentation
slides/       Final presentation deck
report/       One-page project write-up
outputs/      Placeholder folder for generated outputs
