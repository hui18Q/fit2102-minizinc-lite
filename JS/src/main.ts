import {
    catchError,
    fromEvent,
    map,
    merge,
    of,
    scan,
    startWith,
    switchMap,
    type Observable,
} from "rxjs";
import { fromFetch } from "rxjs/fetch";
import { Action, ActiveRequest, Output, State } from "./types";

const modelInput = document.getElementById(
    "model-input",
) as HTMLTextAreaElement;

const runButton = document.getElementById("run-model")!;

const saveButton = document.getElementById("save-solution")!;

const outputEl = document.getElementById("model-output") as HTMLOutputElement;

const initialState: State = {
    model: "",
    request: "none",
};

const initialOutput: Output = {
    result: "",
    error: "",
};

// Editing the model stores it but does not contact the backend.
const modelInput$: Observable<Action<State>> = fromEvent(
    modelInput,
    "input",
).pipe(
    map(event => (event.target as HTMLTextAreaElement).value),
    map(value => state => ({
        ...state,
        model: value,
        request: "none",
    })),
);

// Asks the backend to solve the current model.
const run$: Observable<Action<State>> = fromEvent(runButton, "click").pipe(
    map(() => state => ({
        ...state,
        request: "run",
    })),
);

// Asks the backend to solve the current model and save the solution.
const save$: Observable<Action<State>> = fromEvent(saveButton, "click").pipe(
    map(() => state => ({
        ...state,
        request: "save",
    })),
);

type ApiResponse = Readonly<Record<string, string>>;

// For each request: the endpoint to call and how a successful
// response is shown. Error responses are handled the same way for both.
const endpoints: Readonly<
    Record<
        ActiveRequest,
        { url: string; toOutput: (res: ApiResponse) => Output }
    >
> = {
    run: {
        url: "/api/minizinc",
        toOutput: res => ({ result: JSON.stringify(res), error: "" }),
    },
    save: {
        url: "/api/save",
        toOutput: res =>
            "saved" in res
                ? { result: "Saved: " + res.saved, error: "" }
                : { result: "", error: "Unexpected save response" },
    },
};

// Sends the model to the endpoint for the request and turns the reply
// into an update of the output.
function getOutput(
    model: string,
    request: ActiveRequest,
): Observable<Action<Output>> {
    const body = new URLSearchParams();
    body.set("model", model);
    const { url, toOutput } = endpoints[request];

    return fromFetch<ApiResponse>(url, {
        method: "POST",
        headers: {
            "Content-Type": "application/x-www-form-urlencoded",
        },
        body: body.toString(),
        selector: res => res.json(),
    }).pipe(
        map((res): Action<Output> =>
            "error" in res
                ? () => ({ result: "", error: res.error })
                : () => toOutput(res),
        ),
        // Without this, one failed request (e.g. the backend is not
        // running) would end the output stream for good.
        catchError(err =>
            of((): Output => ({
                result: "",
                error: "Request failed: " + String(err),
            })),
        ),
    );
}

function main() {
    // Every user action updates the same state stream.
    const state$ = merge(modelInput$, run$, save$).pipe(
        scan((state, action) => action(state), initialState),
        startWith(initialState),
    );
    state$.subscribe(console.log);

    // Every state change goes through switchMap, so editing the model
    // cancels a request still in flight and clears the old output,
    // which no longer matches the edited model.
    const output$ = state$.pipe(
        switchMap(state =>
            state.request === "none"
                ? of((): Output => initialOutput)
                : getOutput(state.model, state.request),
        ),
        scan((output, action) => action(output), initialOutput),
        startWith(initialOutput),
    );

    output$.subscribe(output => {
        console.log(output);
        outputEl.value = output.error || output.result;
    });
}

if (typeof window !== "undefined") {
    window.addEventListener("load", main);
}
