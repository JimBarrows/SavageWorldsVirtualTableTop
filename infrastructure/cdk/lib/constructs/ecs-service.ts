import * as cdk from 'aws-cdk-lib';
import * as ecs from 'aws-cdk-lib/aws-ecs';
import * as ec2 from 'aws-cdk-lib/aws-ec2';
import * as logs from 'aws-cdk-lib/aws-logs';
import * as elbv2 from 'aws-cdk-lib/aws-elasticloadbalancingv2';
import { Construct } from 'constructs';

export interface EcsServiceProps {
  cluster: ecs.ICluster;
  vpc: ec2.IVpc;
  subnets: ec2.ISubnet[];
  serviceName: string;
  containerImage: string;
  containerPort: number;
  environment: string;
  memoryLimit?: number;
  cpuLimit?: number;
  desiredCount?: number;
  environmentVariables?: { [key: string]: string };
  secrets?: { [key: string]: ecs.Secret };
  healthCheckPath?: string;
  enableAutoScaling?: boolean;
  minCapacity?: number;
  maxCapacity?: number;
  targetCpuUtilization?: number;
  targetMemoryUtilization?: number;
}

export class EcsService extends Construct {
  public readonly service: ecs.FargateService;
  public readonly taskDefinition: ecs.FargateTaskDefinition;
  public readonly loadBalancer: elbv2.ApplicationLoadBalancer;
  public readonly targetGroup: elbv2.ApplicationTargetGroup;
  public readonly securityGroup: ec2.SecurityGroup;

  constructor(scope: Construct, id: string, props: EcsServiceProps) {
    super(scope, id);

    // Create task definition
    this.taskDefinition = new ecs.FargateTaskDefinition(this, 'TaskDefinition', {
      memoryLimitMiB: props.memoryLimit || 512,
      cpu: props.cpuLimit || 256,
    });

    // Create log group
    const logGroup = new logs.LogGroup(this, 'LogGroup', {
      logGroupName: `/aws/ecs/${props.serviceName}/${props.environment}`,
      retention: props.environment === 'prod' 
        ? logs.RetentionDays.ONE_MONTH 
        : logs.RetentionDays.ONE_WEEK,
      removalPolicy: props.environment === 'prod' 
        ? cdk.RemovalPolicy.RETAIN 
        : cdk.RemovalPolicy.DESTROY,
    });

    // Add container to task definition
    const container = this.taskDefinition.addContainer('main', {
      image: ecs.ContainerImage.fromRegistry(props.containerImage),
      logging: ecs.LogDrivers.awsLogs({
        streamPrefix: props.serviceName,
        logGroup,
      }),
      environment: props.environmentVariables,
      secrets: props.secrets,
    });

    container.addPortMappings({
      containerPort: props.containerPort,
      protocol: ecs.Protocol.TCP,
    });

    // Create security group for ECS service
    this.securityGroup = new ec2.SecurityGroup(this, 'ServiceSecurityGroup', {
      vpc: props.vpc,
      description: `Security group for ${props.serviceName} ECS service`,
      allowAllOutbound: true,
    });

    // Create Application Load Balancer
    this.loadBalancer = new elbv2.ApplicationLoadBalancer(this, 'LoadBalancer', {
      vpc: props.vpc,
      internetFacing: true,
      loadBalancerName: `${props.serviceName}-${props.environment}`,
      vpcSubnets: {
        subnets: props.subnets.filter(subnet => subnet.subnetId), // Public subnets
      },
    });

    // Create target group
    this.targetGroup = new elbv2.ApplicationTargetGroup(this, 'TargetGroup', {
      vpc: props.vpc,
      port: props.containerPort,
      protocol: elbv2.ApplicationProtocol.HTTP,
      targetType: elbv2.TargetType.IP,
      targetGroupName: `${props.serviceName}-${props.environment}`,
      healthCheck: {
        enabled: true,
        path: props.healthCheckPath || '/health',
        interval: cdk.Duration.seconds(30),
        timeout: cdk.Duration.seconds(5),
        healthyThresholdCount: 2,
        unhealthyThresholdCount: 5,
        protocol: elbv2.Protocol.HTTP,
      },
    });

    // Create listener
    this.loadBalancer.addListener('Listener', {
      port: 80,
      protocol: elbv2.ApplicationProtocol.HTTP,
      defaultTargetGroups: [this.targetGroup],
    });

    // Allow ALB to access ECS service
    this.securityGroup.addIngressRule(
      ec2.Peer.securityGroupId(this.loadBalancer.connections.securityGroups[0].securityGroupId),
      ec2.Port.tcp(props.containerPort),
      'Allow ALB to access ECS service'
    );

    // Create ECS service
    this.service = new ecs.FargateService(this, 'Service', {
      cluster: props.cluster,
      taskDefinition: this.taskDefinition,
      serviceName: `${props.serviceName}-${props.environment}`,
      desiredCount: props.desiredCount || 1,
      vpcSubnets: {
        subnets: props.subnets,
      },
      securityGroups: [this.securityGroup],
      assignPublicIp: false,
    });

    // Attach service to target group
    this.service.attachToApplicationTargetGroup(this.targetGroup);

    // Configure auto scaling if enabled
    if (props.enableAutoScaling) {
      const scaling = this.service.autoScaleTaskCount({
        minCapacity: props.minCapacity || 1,
        maxCapacity: props.maxCapacity || 10,
      });

      if (props.targetCpuUtilization) {
        scaling.scaleOnCpuUtilization('CpuScaling', {
          targetUtilizationPercent: props.targetCpuUtilization,
          scaleInCooldown: cdk.Duration.minutes(5),
          scaleOutCooldown: cdk.Duration.minutes(2),
        });
      }

      if (props.targetMemoryUtilization) {
        scaling.scaleOnMemoryUtilization('MemoryScaling', {
          targetUtilizationPercent: props.targetMemoryUtilization,
          scaleInCooldown: cdk.Duration.minutes(5),
          scaleOutCooldown: cdk.Duration.minutes(2),
        });
      }
    }

    // Add tags
    cdk.Tags.of(this).add('Service', props.serviceName);
    cdk.Tags.of(this).add('Environment', props.environment);
  }

  /**
   * Grant the service's task role permissions to access other AWS resources
   */
  public grantTaskRolePermissions(statement: cdk.aws_iam.PolicyStatement): void {
    this.taskDefinition.addToTaskRolePolicy(statement);
  }

  /**
   * Add additional security group rules
   */
  public addSecurityGroupRule(
    peer: ec2.IPeer,
    port: ec2.Port,
    description?: string
  ): void {
    this.securityGroup.addIngressRule(peer, port, description);
  }

  /**
   * Get the service's load balancer DNS name
   */
  public getLoadBalancerDnsName(): string {
    return this.loadBalancer.loadBalancerDnsName;
  }

  /**
   * Get the service's URL
   */
  public getServiceUrl(): string {
    return `http://${this.loadBalancer.loadBalancerDnsName}`;
  }
}