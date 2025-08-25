import * as cdk from 'aws-cdk-lib';
import * as s3 from 'aws-cdk-lib/aws-s3';
import * as cloudfront from 'aws-cdk-lib/aws-cloudfront';
import * as origins from 'aws-cdk-lib/aws-cloudfront-origins';
import * as s3deploy from 'aws-cdk-lib/aws-s3-deployment';
import * as iam from 'aws-cdk-lib/aws-iam';
import { Construct } from 'constructs';

export interface FrontendStackProps extends cdk.StackProps {
  environment: string;
  backendUrl?: string;
  domainName?: string;
}

export class FrontendStack extends cdk.Stack {
  public readonly bucket: s3.Bucket;
  public readonly distribution: cloudfront.Distribution;
  public readonly originAccessIdentity: cloudfront.OriginAccessIdentity;

  constructor(scope: Construct, id: string, props: FrontendStackProps) {
    super(scope, id, props);

    // Create S3 bucket for static website hosting
    this.bucket = new s3.Bucket(this, 'SWVTTFrontendBucket', {
      bucketName: `swvtt-frontend-${props.environment}-${cdk.Aws.ACCOUNT_ID}`,
      websiteIndexDocument: 'index.html',
      websiteErrorDocument: 'error.html',
      publicReadAccess: false,
      blockPublicAccess: s3.BlockPublicAccess.BLOCK_ALL,
      removalPolicy: props.environment === 'prod' ? cdk.RemovalPolicy.RETAIN : cdk.RemovalPolicy.DESTROY,
      autoDeleteObjects: props.environment !== 'prod',
      versioned: props.environment === 'prod',
    });

    // Create Origin Access Identity for CloudFront
    this.originAccessIdentity = new cloudfront.OriginAccessIdentity(this, 'SWVTTOriginAccessIdentity', {
      comment: `OAI for SWVTT ${props.environment} frontend`,
    });

    // Grant CloudFront access to S3 bucket
    this.bucket.addToResourcePolicy(new iam.PolicyStatement({
      actions: ['s3:GetObject'],
      resources: [this.bucket.arnForObjects('*')],
      principals: [this.originAccessIdentity.grantPrincipal],
    }));

    // Create CloudFront distribution
    this.distribution = new cloudfront.Distribution(this, 'SWVTTDistribution', {
      defaultBehavior: {
        origin: new origins.S3Origin(this.bucket, {
          originAccessIdentity: this.originAccessIdentity,
        }),
        viewerProtocolPolicy: cloudfront.ViewerProtocolPolicy.REDIRECT_TO_HTTPS,
        allowedMethods: cloudfront.AllowedMethods.ALLOW_GET_HEAD_OPTIONS,
        cachedMethods: cloudfront.CachedMethods.CACHE_GET_HEAD_OPTIONS,
        compress: true,
        cachePolicy: cloudfront.CachePolicy.CACHING_OPTIMIZED,
      },
      additionalBehaviors: {
        '/api/*': {
          origin: new origins.HttpOrigin(props.backendUrl || 'localhost:3000'),
          viewerProtocolPolicy: cloudfront.ViewerProtocolPolicy.REDIRECT_TO_HTTPS,
          allowedMethods: cloudfront.AllowedMethods.ALLOW_ALL,
          cachePolicy: cloudfront.CachePolicy.CACHING_DISABLED,
          originRequestPolicy: cloudfront.OriginRequestPolicy.ALL_VIEWER,
        },
      },
      errorResponses: [
        {
          httpStatus: 404,
          responseHttpStatus: 200,
          responsePagePath: '/index.html',
          ttl: cdk.Duration.minutes(30),
        },
        {
          httpStatus: 403,
          responseHttpStatus: 200,
          responsePagePath: '/index.html',
          ttl: cdk.Duration.minutes(30),
        },
      ],
      defaultRootObject: 'index.html',
      enableIpv6: true,
      priceClass: props.environment === 'prod' 
        ? cloudfront.PriceClass.PRICE_CLASS_ALL 
        : cloudfront.PriceClass.PRICE_CLASS_100,
      comment: `SWVTT ${props.environment} frontend distribution`,
    });

    // Create deployment for initial static files (if build directory exists)
    const deploymentSourcePath = '../../ui-web/build';
    try {
      new s3deploy.BucketDeployment(this, 'SWVTTDeployment', {
        sources: [s3deploy.Source.asset(deploymentSourcePath)],
        destinationBucket: this.bucket,
        distribution: this.distribution,
        distributionPaths: ['/*'],
      });
    } catch (error) {
      // Deployment source doesn't exist yet - will be deployed separately
      console.log('Note: Frontend build directory not found. Deploy manually after building frontend.');
    }

    // Tag resources appropriately
    cdk.Tags.of(this).add('Project', 'SWVTT');
    cdk.Tags.of(this).add('Environment', props.environment);
    cdk.Tags.of(this).add('Stack', 'Frontend');

    // Export CloudFront distribution information
    new cdk.CfnOutput(this, 'DistributionDomainName', {
      value: this.distribution.distributionDomainName,
      exportName: `${props.environment}-swvtt-frontend-domain`,
      description: 'CloudFront distribution domain name for SWVTT frontend',
    });

    new cdk.CfnOutput(this, 'DistributionId', {
      value: this.distribution.distributionId,
      exportName: `${props.environment}-swvtt-distribution-id`,
      description: 'CloudFront distribution ID for SWVTT frontend',
    });

    new cdk.CfnOutput(this, 'BucketName', {
      value: this.bucket.bucketName,
      exportName: `${props.environment}-swvtt-frontend-bucket`,
      description: 'S3 bucket name for SWVTT frontend',
    });

    new cdk.CfnOutput(this, 'WebsiteURL', {
      value: `https://${this.distribution.distributionDomainName}`,
      exportName: `${props.environment}-swvtt-website-url`,
      description: 'SWVTT website URL',
    });
  }
}