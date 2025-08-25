# SWVTT CDK Infrastructure Setup

This directory contains the AWS CDK infrastructure code for the Savage Worlds Virtual Table Top (SWVTT) application.

## Project Structure

```
infrastructure/cdk/
├── bin/
│   └── cdk.ts              # Main CDK app entry point
├── lib/
│   ├── stacks/
│   │   ├── network-stack.ts    # VPC, subnets, networking
│   │   ├── database-stack.ts   # RDS database infrastructure
│   │   ├── backend-stack.ts    # ECS, ALB, backend services
│   │   ├── frontend-stack.ts   # S3, CloudFront, static hosting
│   │   └── monitoring-stack.ts # CloudWatch, alarms, dashboards
│   └── constructs/
│       ├── ecs-service.ts      # Reusable ECS service construct
│       └── rds-database.ts     # Reusable RDS database construct
├── config/
│   ├── dev.json           # Development environment config
│   ├── qa.json            # QA environment config
│   └── prod.json          # Production environment config
├── test/
│   └── cdk-project-setup.test.js  # Infrastructure unit tests
├── cdk.json              # CDK toolkit configuration
├── package.json          # Node.js dependencies
├── tsconfig.json         # TypeScript configuration
└── README.md            # This file
```

## Prerequisites

Before you begin, ensure you have the following installed:

- **Node.js 18+**: [Download Node.js](https://nodejs.org/)
- **AWS CLI**: [Installation Guide](https://docs.aws.amazon.com/cli/latest/userguide/getting-started-install.html)
- **AWS CDK CLI**: Install globally with `npm install -g aws-cdk`

## AWS Setup

1. **Configure AWS Credentials**:
   ```bash
   aws configure
   # Enter your AWS Access Key ID, Secret Access Key, region, and output format
   ```

2. **Verify AWS Configuration**:
   ```bash
   aws sts get-caller-identity
   # Should return your account details
   ```

3. **Bootstrap CDK** (first time only):
   ```bash
   cdk bootstrap aws://ACCOUNT-NUMBER/REGION
   # Replace ACCOUNT-NUMBER with your AWS account ID and REGION with your preferred region
   ```

## Quick Start

1. **Install Dependencies**:
   ```bash
   npm install
   ```

2. **Build the Project**:
   ```bash
   npm run build
   ```

3. **Run Tests**:
   ```bash
   npm test
   ```

4. **Synthesize CloudFormation Templates**:
   ```bash
   # For development environment (default)
   cdk synth

   # For specific environment
   cdk synth --context environment=qa
   cdk synth --context environment=prod
   ```

5. **Deploy Infrastructure**:
   ```bash
   # Deploy to development (default)
   cdk deploy --all

   # Deploy to specific environment
   cdk deploy --all --context environment=qa
   cdk deploy --all --context environment=prod
   ```

## Environment Configuration

The infrastructure supports three environments: `dev`, `qa`, and `prod`. Each environment has its own configuration file in the `config/` directory.

### Development Environment
- **File**: `config/dev.json`
- **Resources**: Minimal resource allocation for cost savings
- **Features**: Basic monitoring, no advanced security features
- **Database**: Single-AZ RDS instance with 7-day backups
- **Scaling**: No auto-scaling enabled

### QA Environment
- **File**: `config/qa.json`
- **Resources**: Medium resource allocation for testing
- **Features**: Enhanced monitoring, WAF enabled, CloudTrail logging
- **Database**: Single-AZ with performance insights, 14-day backups
- **Scaling**: Basic auto-scaling for load testing

### Production Environment
- **File**: `config/prod.json`
- **Resources**: High-availability setup with redundancy
- **Features**: Full security stack, comprehensive monitoring
- **Database**: Multi-AZ RDS with 30-day backups, deletion protection
- **Scaling**: Advanced auto-scaling and performance optimization

## Available Commands

```bash
# Install dependencies
npm install

# Compile TypeScript to JavaScript
npm run build

# Watch for changes and compile
npm run watch

# Run unit tests
npm test

# List all stacks
cdk ls

# Synthesize CloudFormation templates
cdk synth

# Show differences between deployed stack and current state
cdk diff

# Deploy all stacks
cdk deploy --all

# Deploy specific stack
cdk deploy SWVTT-Network-dev

# Destroy all stacks (be careful!)
cdk destroy --all
```

## Stack Dependencies

The stacks have the following deployment order:

1. **NetworkStack**: Creates VPC, subnets, and networking components
2. **DatabaseStack**: Creates RDS instance (depends on NetworkStack)
3. **BackendStack**: Creates ECS services and load balancer (depends on NetworkStack and DatabaseStack)
4. **FrontendStack**: Creates S3 bucket and CloudFront distribution (depends on BackendStack)
5. **MonitoringStack**: Creates CloudWatch dashboards and alarms (depends on all other stacks)

## Security Considerations

- **Secrets Management**: Database credentials are stored in AWS Secrets Manager
- **Network Security**: Private subnets for databases and applications
- **Encryption**: All data at rest and in transit is encrypted
- **IAM Roles**: Least-privilege access for all resources
- **Security Groups**: Restrictive inbound rules

## Monitoring and Alerts

The monitoring stack provides:

- **CloudWatch Dashboard**: Centralized view of all metrics
- **Automated Alarms**: CPU, memory, response time, and error rate monitoring
- **SNS Notifications**: Email alerts for critical issues
- **Log Aggregation**: Centralized logging for all services

## Cost Optimization

- **Environment-Specific Sizing**: Resources scaled appropriately per environment
- **Auto-Scaling**: Automatic scaling based on demand
- **Spot Instances**: Consider using Spot instances for non-production workloads
- **Resource Cleanup**: Automated cleanup of unused resources

## Troubleshooting

### Common Issues

1. **Bootstrap Required**:
   ```
   Error: Need to perform AWS CDK bootstrap
   ```
   **Solution**: Run `cdk bootstrap` in your target region

2. **Permission Denied**:
   ```
   Error: Access Denied
   ```
   **Solution**: Verify AWS credentials and IAM permissions

3. **Resource Conflicts**:
   ```
   Error: Resource already exists
   ```
   **Solution**: Check for existing resources or use different environment name

### Debug Commands

```bash
# Enable verbose logging
cdk deploy --all --verbose

# Show CloudFormation events
aws cloudformation describe-stack-events --stack-name SWVTT-Network-dev

# Check CDK context
cdk context

# Clear CDK context
cdk context --clear
```

## Contributing

1. Make changes to the TypeScript files
2. Run tests: `npm test`
3. Build: `npm run build`
4. Test synthesis: `cdk synth`
5. Deploy to development: `cdk deploy --all --context environment=dev`

## Additional Resources

- [AWS CDK Documentation](https://docs.aws.amazon.com/cdk/)
- [CDK API Reference](https://docs.aws.amazon.com/cdk/api/latest/)
- [AWS Well-Architected Framework](https://aws.amazon.com/architecture/well-architected/)
- [SWVTT Project Documentation](../../README.md)