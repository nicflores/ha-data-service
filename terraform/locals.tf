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
  cpu            = 256
  memory         = 512
}
