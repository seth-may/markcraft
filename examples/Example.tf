# AWS Infrastructure for MarkCraft Backend
terraform {
  required_version = ">= 1.6"
  required_providers {
    aws = { source = "hashicorp/aws", version = "~> 5.0" }
  }
  backend "s3" {
    bucket = "markcraft-terraform-state"
    key    = "prod/terraform.tfstate"
    region = "eu-west-3"
  }
}

provider "aws" {
  region = var.region
  default_tags { tags = { Project = "MarkCraft", Env = var.env } }
}

variable "region"   { default = "eu-west-3" }
variable "env"      { default = "prod" }
variable "app_name" { default = "markcraft-api" }

# VPC
module "vpc" {
  source  = "terraform-aws-modules/vpc/aws"
  version = "5.5.0"
  name    = "${var.app_name}-vpc"
  cidr    = "10.0.0.0/16"
  azs             = ["${var.region}a", "${var.region}b"]
  private_subnets = ["10.0.1.0/24", "10.0.2.0/24"]
  public_subnets  = ["10.0.101.0/24", "10.0.102.0/24"]
  enable_nat_gateway = true
  single_nat_gateway = true
}

# ECS Fargate Cluster
resource "aws_ecs_cluster" "main" {
  name = "${var.app_name}-cluster"
  setting { name = "containerInsights"; value = "enabled" }
}

resource "aws_ecs_task_definition" "app" {
  family                   = var.app_name
  network_mode             = "awsvpc"
  requires_compatibilities = ["FARGATE"]
  cpu                      = 256
  memory                   = 512
  execution_role_arn       = aws_iam_role.ecs_exec.arn

  container_definitions = jsonencode([{
    name  = var.app_name
    image = "${aws_ecr_repository.app.repository_url}:latest"
    portMappings = [{ containerPort = 8080, protocol = "tcp" }]
    logConfiguration = {
      logDriver = "awslogs"
      options   = {
        "awslogs-group"  = aws_cloudwatch_log_group.app.name
        "awslogs-region" = var.region
      }
    }
    environment = [
      { name = "APP_ENV", value = var.env },
      { name = "PORT",    value = "8080" }
    ]
  }])
}

# ALB
resource "aws_lb" "main" {
  name               = "${var.app_name}-alb"
  internal           = false
  load_balancer_type = "application"
  subnets            = module.vpc.public_subnets
  security_groups    = [aws_security_group.alb.id]
}

# Outputs
output "alb_dns"     { value = aws_lb.main.dns_name }
output "cluster_arn" { value = aws_ecs_cluster.main.arn }
