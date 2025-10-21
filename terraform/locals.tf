locals {
  region      = "us-east-1"
  environment = "dev"
  profile     = "default"

  tags = {
    Name        = local.app_name
    Environment = local.environment
  }

  app_name       = "${local.environment}-${local.region}-hs-data-service"
  container_port = 8080
  cpu            = 2048
  memory         = 4096

  # env vars
  s3_bucket                = "your-bucket"
  s3_prefix                = "financial-data"
  yahoo_finance_base_url   = "https://query2.finance.yahoo.com/v8/finance/chart"
  yahoo_finance_lookup_url = "https://query1.finance.yahoo.com/v1/finance/lookup"

}
