output "ecr_repository_url" {
  description = "ECR repository URL"
  value       = aws_ecr_repository.app.repository_url
}

# output "ecs_cluster_name" {
#   description = "ECS cluster name"
#   value       = aws_ecs_cluster.main.name
# }

# output "ecs_service_name" {
#   description = "ECS service name"
#   value       = aws_ecs_service.app.name
# }

# output "cloudwatch_log_group" {
#   description = "CloudWatch log group name"
#   value       = aws_cloudwatch_log_group.app.name
# }
