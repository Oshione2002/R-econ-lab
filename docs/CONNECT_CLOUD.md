# Posit Connect Cloud deployment

The application is prepared for Git-backed deployment to Posit Connect Cloud.

## Publish

1. Sign in to Posit Connect Cloud with GitHub.
2. Install or authorize the Posit Connect Cloud GitHub App for `Oshione2002/R-econ-lab`.
3. Select **Publish** and choose **Shiny**.
4. Select repository `Oshione2002/R-econ-lab`.
5. Select branch `main`.
6. Select `app.R` as the primary file.
7. Confirm that `manifest.json` is detected in the repository root.
8. Publish and inspect the build log.

## Runtime policy

Uploaded data and generated files are temporary in the hosted application. Package installation and arbitrary custom R execution are disabled by default. Set the relevant environment variables only in a controlled environment.
