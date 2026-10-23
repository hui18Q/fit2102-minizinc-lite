// A request the user has asked the backend to handle.
export type ActiveRequest = "run" | "save";

// "none" means the model changed and nothing has been asked yet.
// One field (instead of separate run/save flags) means a run and a
// save can never both be pending at once.
export type Request = "none" | ActiveRequest;

export type State = Readonly<{
    model: string;
    request: Request;
}>;

export type Output = Readonly<
    { result: string; error: "" } | { result: ""; error: string }
>;

export type Action<T> = (_: T) => T;
