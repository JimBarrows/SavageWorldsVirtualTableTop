import * as cdk from 'aws-cdk-lib';
import * as ecs from 'aws-cdk-lib/aws-ecs';
import * as ec2 from 'aws-cdk-lib/aws-ec2';
import * as elbv2 from 'aws-cdk-lib/aws-elasticloadbalancingv2';
import * as logs from 'aws-cdk-lib/aws-logs';
import * as iam from 'aws-cdk-lib/aws-iam';
import * as secretsmanager from 'aws-cdk-lib/aws-secretsmanager';
import { Construct } from 'constructs';

export interface BackendStackProps extends cdk.StackProps {
  environment: string;
  vpc: ec2.IVpc;
  databaseSecret: secretsmanager.ISecret;
  databaseEndpoint: string;
  privateSubnets: ec2.ISubnet[];
  publicSubnets: ec2.ISubnet[];
}

export class BackendStack extends cdk.Stack {
  public readonly cluster: ecs.Cluster;
  public readonly service: ecs.FargateService;
  public readonly loadBalancer: elbv2.ApplicationLoadBalancer;

  constructor(scope: Construct, id: string, props: BackendStackProps) {
    super(scope, id, props);

    // Create ECS cluster
    this.cluster = new ecs.Cluster(this, 'SWVTTCluster', {
      vpc: props.vpc,
      clusterName: `swvtt-${props.environment}`,
      containerInsights: props.environment === 'prod',
    });

    // Create task definition
    const taskDefinition = new ecs.FargateTaskDefinition(this, 'SWVTTTaskDefinition', {
      memoryLimitMiB: props.environment === 'prod' ? 2048 : 512,
      cpu: props.environment === 'prod' ? 1024 : 256,
    });

    // Create log group for application logs
    const logGroup = new logs.LogGroup(this, 'SWVTTLogGroup', {
      logGroupName: `/aws/ecs/swvtt/${props.environment}`,
      retention: props.environment === 'prod' ? logs.RetentionDays.ONE_MONTH : logs.RetentionDays.ONE_WEEK,
    });

    // Add container to task definition
    const container = taskDefinition.addContainer('swvtt-backend', {
      image: ecs.ContainerImage.fromRegistry('nginx:latest'), // Placeholder - replace with actual image
      logging: ecs.LogDrivers.awsLogs({
        streamPrefix: 'swvtt-backend',
        logGroup,
      }),
      environment: {
        NODE_ENV: props.environment === 'prod' ? 'production' : 'development',
        APP_ENV: props.environment,
      },
      secrets: {
        DATABASE_URL: ecs.Secret.fromSecretsManager(props.databaseSecret),
      },
    });

    container.addPortMappings({
      containerPort: 3000,
      protocol: ecs.Protocol.TCP,
    });

    // Grant task definition access to secrets
    props.databaseSecret.grantRead(taskDefinition.taskRole);

    // Create security group for ECS service
    const ecsSecurityGroup = new ec2.SecurityGroup(this, 'ECSSecurityGroup', {
      vpc: props.vpc,
      description: 'Security group for SWVTT ECS service',
      allowAllOutbound: true,
    });

    // Create Application Load Balancer
    this.loadBalancer = new elbv2.ApplicationLoadBalancer(this, 'SWVTTLoadBalancer', {
      vpc: props.vpc,
      internetFacing: true,
      vpcSubnets: {
        subnets: props.publicSubnets,
      },
    });

    // Create target group
    const targetGroup = new elbv2.ApplicationTargetGroup(this, 'SWVTTTargetGroup', {
      vpc: props.vpc,
      port: 3000,
      protocol: elbv2.ApplicationProtocol.HTTP,
      targetType: elbv2.TargetType.IP,
      healthCheck: {
        enabled: true,
        path: '/health',
        interval: cdk.Duration.seconds(30),
        timeout: cdk.Duration.seconds(5),
        healthyThresholdCount: 2,
        unhealthyThresholdCount: 5,
      },
    });

    // Create listener
    const listener = this.loadBalancer.addListener('SWVTTListener', {
      port: 80,
      protocol: elbv2.ApplicationProtocol.HTTP,
      defaultTargetGroups: [targetGroup],
    });

    // Allow ALB to access ECS service
    ecsSecurityGroup.addIngressRule(
      ec2.Peer.securityGroupId(this.loadBalancer.connections.securityGroups[0].securityGroupId),
      ec2.Port.tcp(3000),
      'Allow ALB to access ECS service'
    );

    // Create ECS service
    this.service = new ecs.FargateService(this, 'SWVTTService', {
      cluster: this.cluster,
      taskDefinition,
      desiredCount: props.environment === 'prod' ? 2 : 1,
      vpcSubnets: {
        subnets: props.privateSubnets,
      },
      securityGroups: [ecsSecurityGroup],
      assignPublicIp: false,
    });

    // Attach service to target group
    this.service.attachToApplicationTargetGroup(targetGroup);

    // Configure auto scaling for production
    if (props.environment === 'prod') {
      const scaling = this.service.autoScaleTaskCount({
        minCapacity: 2,
        maxCapacity: 10,
      });

      scaling.scaleOnCpuUtilization('CpuScaling', {
        targetUtilizationPercent: 70,
      });

      scaling.scaleOnMemoryUtilization('MemoryScaling', {
        targetUtilizationPercent: 80,
      });
    }

    // Tag resources appropriately
    cdk.Tags.of(this).add('Project', 'SWVTT');
    cdk.Tags.of(this).add('Environment', props.environment);
    cdk.Tags.of(this).add('Stack', 'Backend');

    // Export load balancer DNS name
    new cdk.CfnOutput(this, 'LoadBalancerDNSName', {
      value: this.loadBalancer.loadBalancerDnsName,
      exportName: `${props.environment}-swvtt-backend-dns`,
      description: 'Load balancer DNS name for SWVTT backend',
    });

    new cdk.CfnOutput(this, 'ServiceName', {
      value: this.service.serviceName,
      exportName: `${props.environment}-swvtt-service-name`,
      description: 'ECS service name for SWVTT backend',
    });
  }
}