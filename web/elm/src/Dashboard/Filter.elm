module Dashboard.Filter exposing (filterGroups)

import Concourse exposing (DatabaseID)
import Concourse.PipelineStatus
    exposing
        ( PipelineStatus(..)
        , StatusDetails(..)
        , equal
        , isRunning
        )
import Dashboard.Group.Models exposing (Card(..), Group, Pipeline)
import Dashboard.Pipeline as Pipeline
import Dict exposing (Dict)
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Step(..)
        , backtrackable
        , chompWhile
        , end
        , getChompedString
        , keyword
        , loop
        , map
        , oneOf
        , run
        , spaces
        , succeed
        , symbol
        )
import Routes
import Set exposing (Set)
import Simple.Fuzzy


type alias Filter =
    { negate : Bool
    , groupFilter : GroupFilter
    }


filterGroups :
    { pipelineJobs : Dict Concourse.DatabaseID (List Concourse.JobIdentifier)
    , jobs : Dict ( Concourse.DatabaseID, String ) Concourse.Job
    , query : String
    , teams : List Concourse.Team
    , pipelines : Dict String (List Pipeline)
    , dashboardView : Routes.DashboardView
    , favoritedPipelines : Set DatabaseID
    }
    -> List Group
filterGroups { pipelineJobs, jobs, query, teams, pipelines, dashboardView, favoritedPipelines } =
    let
        groupsToFilter =
            teams
                |> List.map (\t -> ( t.name, [] ))
                |> Dict.fromList
                |> Dict.union pipelines
                |> Dict.toList
                |> List.map
                    (\( t, p ) ->
                        { teamName = t
                        , cards =
                            List.filter (prefilter dashboardView favoritedPipelines) p
                                |> List.map PipelineCard
                        }
                    )
    in
    parseFilters query |> List.foldr (runFilter jobs pipelineJobs) groupsToFilter


prefilter : Routes.DashboardView -> Set DatabaseID -> Pipeline -> Bool
prefilter view favoritedPipelines p =
    case view of
        Routes.ViewNonArchivedPipelines ->
            not p.archived || Set.member p.id favoritedPipelines

        _ ->
            True


runFilter :
    Dict ( Concourse.DatabaseID, String ) Concourse.Job
    -> Dict Concourse.DatabaseID (List Concourse.JobIdentifier)
    -> Filter
    -> List Group
    -> List Group
runFilter jobs existingJobs f =
    let
        negater =
            if f.negate then
                not

            else
                identity
    in
    case f.groupFilter of
        Team teamName ->
            List.filter (.teamName >> Simple.Fuzzy.match teamName >> negater)

        Card c ->
            List.map
                (\g ->
                    { g
                        | cards =
                            g.cards
                                |> List.filter (cardFilter c jobs existingJobs >> negater)
                    }
                )
                >> List.filter (.cards >> List.isEmpty >> not)


lookupJob : Dict ( Concourse.DatabaseID, String ) Concourse.Job -> Concourse.JobIdentifier -> Maybe Concourse.Job
lookupJob jobs jobId =
    jobs
        |> Dict.get ( jobId.pipelineId, jobId.jobName )


cardFilter :
    CardFilter
    -> Dict ( Concourse.DatabaseID, String ) Concourse.Job
    -> Dict Concourse.DatabaseID (List Concourse.JobIdentifier)
    -> Card
    -> Bool
cardFilter cf jobs existingJobs card =
    case cf of
        FuzzyName term ->
            case card of
                PipelineCard { name } ->
                    Simple.Fuzzy.match term name

                InstanceGroupCard { name } ->
                    Simple.Fuzzy.match term name

        Pipeline pf ->
            case card of
                PipelineCard p ->
                    pipelineFilter pf jobs existingJobs p

                InstanceGroupCard { pipelines } ->
                    -- -- Maybe filter down the list of pipelines
                    -- pipelines
                    --     |> List.any (pipelineFilter pf jobs existingJobs)
                    False


pipelineFilter :
    PipelineFilter
    -> Dict ( Concourse.DatabaseID, String ) Concourse.Job
    -> Dict Concourse.DatabaseID (List Concourse.JobIdentifier)
    -> Pipeline
    -> Bool
pipelineFilter pf jobs existingJobs pipeline =
    let
        jobsForPipeline =
            existingJobs
                |> Dict.get pipeline.id
                |> Maybe.withDefault []
                |> List.filterMap (lookupJob jobs)
    in
    case pf of
        Status sf ->
            case sf of
                PipelineStatus ps ->
                    pipeline |> Pipeline.pipelineStatus jobsForPipeline |> equal ps

                PipelineRunning ->
                    pipeline |> Pipeline.pipelineStatus jobsForPipeline |> isRunning


parseFilters : String -> List Filter
parseFilters =
    run
        (loop [] <|
            \revFilters ->
                oneOf
                    [ end
                        |> map (\_ -> Done (List.reverse revFilters))
                    , filter
                        |> map (\f -> Loop (f :: revFilters))
                    ]
        )
        >> Result.withDefault []


filter : Parser Filter
filter =
    oneOf
        [ succeed (Filter True) |. spaces |. symbol "-" |= groupFilter |. spaces
        , succeed (Filter False) |. spaces |= groupFilter |. spaces
        ]


type GroupFilter
    = Team String
    | Card CardFilter


type CardFilter
    = FuzzyName String
    | Pipeline PipelineFilter


type PipelineFilter
    = Status StatusFilter


groupFilter : Parser GroupFilter
groupFilter =
    oneOf
        [ backtrackable teamFilter
        , backtrackable statusFilter
        , succeed (FuzzyName >> Card) |= parseWord
        ]


parseWord : Parser String
parseWord =
    getChompedString
        (chompWhile
            (\c -> c /= ' ' && c /= '\t' && c /= '\n' && c /= '\u{000D}')
        )


type StatusFilter
    = PipelineStatus PipelineStatus
    | PipelineRunning


teamFilter : Parser GroupFilter
teamFilter =
    succeed Team
        |. keyword "team"
        |. symbol ":"
        |. spaces
        |= parseWord


statusFilter : Parser GroupFilter
statusFilter =
    succeed (Status >> Pipeline >> Card)
        |. keyword "status"
        |. symbol ":"
        |. spaces
        |= pipelineStatus


pipelineStatus : Parser StatusFilter
pipelineStatus =
    oneOf
        [ map (\_ -> PipelineStatus PipelineStatusPaused) (keyword "paused")
        , map (\_ -> PipelineStatus <| PipelineStatusAborted Running)
            (keyword "aborted")
        , map (\_ -> PipelineStatus <| PipelineStatusErrored Running)
            (keyword "errored")
        , map (\_ -> PipelineStatus <| PipelineStatusFailed Running)
            (keyword "failed")
        , map (\_ -> PipelineStatus <| PipelineStatusPending False)
            (keyword "pending")
        , map (\_ -> PipelineStatus <| PipelineStatusSucceeded Running)
            (keyword "succeeded")
        , map (\_ -> PipelineRunning) (keyword "running")
        ]
