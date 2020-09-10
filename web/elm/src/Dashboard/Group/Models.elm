module Dashboard.Group.Models exposing (Card(..), Group, InstanceGroup, Pipeline)

-- TODO: maybe rename this?


type alias Group =
    { cards : List Card
    , teamName : String
    }


type Card
    = PipelineCard Pipeline
    | InstanceGroupCard InstanceGroup


type alias Pipeline =
    { id : Int
    , name : String
    , teamName : String
    , public : Bool
    , isToggleLoading : Bool
    , isVisibilityLoading : Bool
    , paused : Bool
    , archived : Bool
    , stale : Bool
    , jobsDisabled : Bool
    }


type alias InstanceGroup =
    { name : String

    --, teamName : String
    , pipelines : List Pipeline
    }
