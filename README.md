# MAS-Motus Detection Retriever

A wildlife tracking dashboard built for the [Mecklenburg Audubon Society](https://www.meckbirds.org/motus). Detection data is retrieved from a Motus receiver on UNC Charlotte's campus, processed into JSON, and displayed as a live filterable card grid embedded on the MAS webpage.

![Status](https://img.shields.io/badge/status-active-brightgreen) ![HTML](https://img.shields.io/badge/language-HTML-lightgrey) ![R](https://img.shields.io/badge/language-R-276DC3)

---

## Contents

- [Overview](#overview)
- [Update schedule](#update-schedule)
- [Repository structure](#repository-structure)
- [How it works](#how-it-works)
- [Configuration & secrets](#configuration--secrets)
- [Data format](#data-format)
- [HTML embeds](#html-embeds)

---

## Overview

This repository powers the Motus detection display at [meckbirds.org/motus](https://www.meckbirds.org/motus). A Motus receiver located on the UNC Charlotte campus automatically logs detections of radio-tagged wildlife as they pass through the area. This repo retrieves that data, processes it, and presents it in a browser-based viewer.

Detections are tagged as first-visit or returning based on tag deploy ID, and can be filtered by season, year, visit type, and species.

---

## Update schedule
 
The workflow runs automatically on a schedule that mirrors the Motus migration seasons. All runs occur at 6:00 AM UTC.
 
| Period | Frequency | Months |
|---|---|---|
| Spring migration | Weekly (Sundays) | March – May |
| Fall migration | Weekly (Sundays) | August – November |
| Off-season | Monthly (1st of month) | December – February, June – July |
 
The workflow can also be triggered manually at any time from the Actions tab in GitHub.

---

## Repository structure

```
MAS-Motus/
├── .github/
│   └── workflows/        # GitHub Actions — automated data refresh
├── data/
│   └── detections.json   # Processed detection data (auto-updated)
├── html-embeds/
│   └── detections.html   # Self-contained viewer embedded on the MAS site
└── scripts/
    └── *.R               # R scripts for fetching and processing Motus data
```

> **Git LFS:** The `.motus` receiver file is stored using [Git Large File Storage](https://git-lfs.com). If you clone this repository,
> make sure Git LFS is installed (`git lfs install`) before pulling, otherwise the file will download as a pointer rather than the actual data.

---

## How it works

1. A GitHub Actions workflow runs on a schedule and triggers the R scripts in `scripts/`
2. The R scripts query the Motus API for new detections from the UNC Charlotte receiver
3. Results are written to `data/detections.json` and committed back to the repository
4. `data/detections.html` fetches the JSON at page load and renders the card grid

The viewer reads the JSON from this URL:

```js
const JSON_URL = 'https://raw.githubusercontent.com/sgagne-code/MAS-Motus/main/data/detections.json';
```

---

## Configuration & secrets

The R scripts require Motus login credentials to retrieve detection data. This can be anyone's login information as long as it is valid. These are stored as GitHub Actions secrets and are never committed to the repository.

To update an existing credential:
 
1. Go to the repository on GitHub
2. Navigate to **Settings → Secrets and variables → Actions**
3. Click the name of the secret you want to change
4. Click **Update**, enter the new value, and confirm

To add a new credential:
 
1. Go to the repository on GitHub
2. Navigate to **Settings → Secrets and variables → Actions**
3. Click **New repository secret**, enter the name and value, and confirm
4. Reference it in the workflow file as `${{ secrets.YOUR_SECRET_NAME }}`


Current secrets used by the workflow:

| Secret name | Description |
|---|---|
| `MOTUS_USER` | Motus username |
| `MOTUS_PASS` | Motus password |

---

## Data format

The viewer expects `data/detections.json` to follow this structure:

```json
{
  "detections": [
    {
      "commonName": "...",
      "scientificName": "...",
      "date": "2024-09-15T14:32:00Z",
      "tagId": "...",
      "tagDeployID": "...",
      "project": "...",
      "projectId": "...",
      "sex": "M or F",
      "age": "ad or imm",
      "locationName": "...",
      "tagDepLat": 00.000,
      "tagDepLon": -00.000
    }
  ]
}
```

---

## HTML embeds
The files in html-embeds/ are not executed by the repository. They serve as a version-controlled 
reference for code that has been manually copied into Wix's HTML embed elements on the MAS webpage. 
Each file corresponds to a labeled embed element on the site. To update the live page, copy the relevant 
file's contents and paste it into the matching embed element in the Wix editor.

---

Fields with missing or null values fall back to placeholder text and do not cause errors in the viewer.
