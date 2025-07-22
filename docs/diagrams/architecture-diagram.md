# Architecture Diagrams

This document contains architecture diagrams for the Savage Worlds Virtual Table Top application.

## System Architecture Overview

```mermaid
graph TB
    subgraph "Client Layer"
        B[Web Browser]
        RApp[React Application]
        B --> RApp
    end
    
    subgraph "Frontend Architecture"
        RApp --> Router[React Router v6]
        Router --> Pages[Page Components]
        Pages --> Components[UI Components]
        Components --> Props[PropTypes Validation]
        
        RApp --> Amplify[AWS Amplify SDK]
        Amplify --> Auth[Amplify Auth]
        Amplify --> API[Amplify API]
        Amplify --> Storage[Amplify Storage]
    end
    
    subgraph "AWS Cloud Services"
        API --> AppSync[AWS AppSync<br/>GraphQL API]
        Auth --> Cognito[AWS Cognito<br/>User Pools]
        Storage --> S3[AWS S3<br/>Private Storage]
        
        AppSync --> DDB[DynamoDB]
        AppSync --> Lambda[Lambda Resolvers<br/>Optional]
    end
    
    subgraph "Development Tools"
        Jest[Jest<br/>Unit Tests]
        Cucumber[Cucumber.js<br/>BDD Tests]
        Storybook[Storybook<br/>Component Docs]
        
        Jest --> RApp
        Cucumber --> Selenium[Selenium WebDriver]
        Selenium --> B
        Storybook --> Components
    end
```

## Component Architecture

```mermaid
graph TD
    App[App.js<br/>Main Component] --> Header[Header<br/>Navigation]
    App --> Routes[Routes]
    
    Routes --> PLList[PlotPointList<br/>Home Page]
    Routes --> PLAdd[PlotPointAdd<br/>Create Page]
    Routes --> PLEdit[PlotPointEdit<br/>Edit Page]
    
    PLAdd --> PLForm[PlotPointForm<br/>Complex Form Component]
    PLEdit --> PLForm
    
    PLForm --> BasicRules[BasicRules<br/>Component]
    PLForm --> SettingRules[SettingRules<br/>Component]
    PLForm --> Skills[Skills<br/>Component]
    PLForm --> Edges[Edges<br/>Component]
    PLForm --> Hindrances[Hindrances<br/>Component]
    PLForm --> Gear[Gear<br/>Component]
    PLForm --> Powers[Powers<br/>Component]
    PLForm --> Races[Races<br/>Component]
    PLForm --> Beasts[Beasts<br/>Component]
    PLForm --> Characters[Characters<br/>Component]
    
    Gear --> GearTypes{Gear Types}
    GearTypes --> Armor[Armor Editor]
    GearTypes --> Weapons[Weapons Editor]
    GearTypes --> Items[Items Editor]
    
    Characters --> CharSheet[Character Sheet<br/>Component]
    CharSheet --> CharTraits[Traits Editor]
    CharSheet --> CharSkills[Skills Selector]
    CharSheet --> CharGear[Gear Selector]
```

## Data Flow Architecture

```mermaid
graph LR
    subgraph "Component State"
        CS[Component State]
        Props[Props]
    end
    
    subgraph "API Layer"
        GraphQL[GraphQL Operations]
        Amplify[AWS Amplify]
    end
    
    subgraph "Backend"
        AppSync[AppSync]
        DDB[(DynamoDB)]
    end
    
    CS -->|setState| CS
    Props -->|Data Down| CS
    CS -->|Events Up| Props
    
    CS -->|User Action| GraphQL
    GraphQL -->|API.graphql| Amplify
    Amplify -->|HTTPS| AppSync
    AppSync -->|Query/Mutation| DDB
    
    DDB -->|Response| AppSync
    AppSync -->|JSON| Amplify
    Amplify -->|Promise| GraphQL
    GraphQL -->|setState| CS
```

## Directory Structure

```mermaid
graph TD
    Root[SavageWorldsVirtualTableTop/]
    Root --> UIWeb[ui-web/]
    Root --> Docs[docs/]
    
    UIWeb --> Src[src/]
    UIWeb --> Features[features/]
    UIWeb --> Stories[stories/]
    UIWeb --> Amplify[amplify/]
    UIWeb --> Public[public/]
    
    Src --> Components[components/<br/>Reusable UI]
    Src --> Pages[pages/<br/>Route Pages]
    Src --> Models[models/<br/>Data Models]
    Src --> GraphQLOps[graphql/<br/>Auto-generated]
    Src --> PropTypeDefs[propTypes/<br/>Type Definitions]
    
    Features --> Steps[step_definitions/]
    Features --> Support[support/]
    Features --> FeatureFiles[*.feature]
    
    Amplify --> Backend[backend/]
    Backend --> APIConfig[api/]
    Backend --> AuthConfig[auth/]
    Backend --> StorageConfig[storage/]
    
    Docs --> Diagrams[diagrams/<br/>This Documentation]
```

## Deployment Architecture

```mermaid
graph TB
    subgraph "Development"
        LocalDev[Local Development<br/>npm start]
        DevBranch[Feature Branch]
    end
    
    subgraph "Build Process"
        ReactBuild[React Build<br/>npm run build]
        AmplifyBuild[Amplify Build<br/>amplify push]
    end
    
    subgraph "AWS Deployment"
        S3Hosting[S3 + CloudFront<br/>Static Hosting]
        AmplifyHosting[Amplify Hosting<br/>Alternative]
        APIDeployment[AppSync API<br/>GraphQL Endpoint]
        CognitoDeployment[Cognito<br/>User Auth]
        DDBTables[DynamoDB<br/>Data Tables]
    end
    
    LocalDev --> DevBranch
    DevBranch --> ReactBuild
    ReactBuild --> BuildArtifacts[Build Artifacts<br/>dist/]
    
    BuildArtifacts --> S3Hosting
    BuildArtifacts --> AmplifyHosting
    
    AmplifyBuild --> APIDeployment
    AmplifyBuild --> CognitoDeployment
    AmplifyBuild --> DDBTables
```

## Technology Stack Layers

```mermaid
graph TD
    subgraph "Frontend Technologies"
        React[React 18.2.0]
        Router[React Router 6.4.4]
        Bootstrap[Bootstrap React Components]
        FontAwesome[FontAwesome Icons]
        PropTypes[PropTypes Validation]
    end
    
    subgraph "State Management"
        ComponentState[Component State]
        LocalState[Local State Only]
        Note[Redux installed but unused]
    end
    
    subgraph "Backend Integration"
        AmplifySDK[AWS Amplify 5.0.4]
        GraphQLClient[GraphQL Client]
        CognitoSDK[Cognito SDK]
    end
    
    subgraph "AWS Services"
        AppSync[AppSync GraphQL]
        Cognito[Cognito Auth]
        DynamoDB[DynamoDB]
        S3[S3 Storage]
    end
    
    subgraph "Development Tools"
        CreateReactApp[Create React App]
        Jest[Jest Testing]
        Cucumber[Cucumber BDD]
        Storybook[Storybook]
        Docker[Docker Support]
    end
    
    React --> ComponentState
    React --> AmplifySDK
    AmplifySDK --> AppSync
    AmplifySDK --> Cognito
    AmplifySDK --> S3
    AppSync --> DynamoDB
```

## Key Architectural Decisions

1. **Monolithic Frontend**: Single React application handles all functionality
2. **AWS-First Backend**: Leverages AWS managed services exclusively
3. **GraphQL API**: All data operations through AppSync GraphQL
4. **Component State**: No global state management (Redux unused)
5. **Class Components**: Primarily class-based React components
6. **BDD Testing**: Cucumber for integration testing
7. **Auto-generated Code**: GraphQL operations generated by Amplify